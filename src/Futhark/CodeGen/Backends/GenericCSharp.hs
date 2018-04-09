{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, LambdaCase #-}
-- | A generic C# code generator which is polymorphic in the type
-- of the operations.  Concretely, we use this to handle both
-- sequential and OpenCL C# code.
module Futhark.CodeGen.Backends.GenericCSharp
  ( compileProg
  , Constructor (..)
  , emptyConstructor

  , assignScalarPointer
  , assignArrayPointer
  , toIntPtr
  , compileName
  , compileDim
  , compileExp
  , compileCode
  , compilePrimValue
  , compilePrimType
  , compilePrimTypeExt
  , compilePrimTypeToAST
  , compilePrimTypeToASText
  , contextFinalInits
  , debugReport

  , Operations (..)
  , defaultOperations

  , unpackDim

  , CompilerM (..)
  , OpCompiler
  , WriteScalar
  , ReadScalar
  , Allocate
  , Copy
  , StaticArray
  , EntryOutput
  , EntryInput

  , CompilerEnv(..)
  , CompilerState(..)
  , stm
  , stms
  , atInit
  , addMemberDecl
  , beforeParse
  , collect'
  , collect
  , simpleCall
  , simpleInitClass
  , parametrizedCall

  , copyMemoryDefaultSpace
  , consoleErrorWrite
  , consoleErrorWriteLine
  , consoleWrite
  , consoleWriteLine

  , publicName
  , sizeOf
  , funDef
  ) where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS
import Control.Arrow((&&&))
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as M

import Futhark.Representation.Primitive hiding (Bool)
import Futhark.MonadFreshNames
import Futhark.Representation.AST.Syntax (Space(..))
import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.CodeGen.Backends.GenericCSharp.AST
import Futhark.CodeGen.Backends.GenericCSharp.Options
import Futhark.CodeGen.Backends.GenericCSharp.Definitions
import Futhark.Util.Pretty(pretty)
import Futhark.Util (zEncodeString)
import Futhark.Representation.AST.Attributes (builtInFunctions)

-- | A substitute expression compiler, tried before the main
-- compilation function.
type OpCompiler op s = op -> CompilerM op s ()

-- | Write a scalar to the given memory block with the given index and
-- in the given memory space.
type WriteScalar op s = VName -> CSExp -> PrimType -> Imp.SpaceId -> CSExp
                        -> CompilerM op s ()

-- | Read a scalar from the given memory block with the given index and
-- in the given memory space.
type ReadScalar op s = VName -> CSExp -> PrimType -> Imp.SpaceId
                       -> CompilerM op s CSExp

-- | Allocate a memory block of the given size in the given memory
-- space, saving a reference in the given variable name.
type Allocate op s = VName -> CSExp -> Imp.SpaceId
                     -> CompilerM op s ()

-- | Copy from one memory block to another.
type Copy op s = VName -> CSExp -> Imp.Space ->
                 VName -> CSExp -> Imp.Space ->
                 CSExp -> PrimType ->
                 CompilerM op s ()

-- | Create a static array of values - initialised at load time.
type StaticArray op s = VName -> Imp.SpaceId -> PrimType -> [PrimValue] -> CompilerM op s ()

-- | Construct the CSthon array being returned from an entry point.
type EntryOutput op s = VName -> Imp.SpaceId ->
                        PrimType -> Imp.Signedness ->
                        [Imp.DimSize] ->
                        CompilerM op s CSExp

-- | Unpack the array being passed to an entry point.
type EntryInput op s = VName -> Imp.MemSize -> Imp.SpaceId ->
                       PrimType -> Imp.Signedness ->
                       [Imp.DimSize] ->
                       CSExp ->
                       CompilerM op s ()


data Operations op s = Operations { opsWriteScalar :: WriteScalar op s
                                  , opsReadScalar :: ReadScalar op s
                                  , opsAllocate :: Allocate op s
                                  , opsCopy :: Copy op s
                                  , opsStaticArray :: StaticArray op s
                                  , opsCompiler :: OpCompiler op s
                                  , opsEntryOutput :: EntryOutput op s
                                  , opsEntryInput :: EntryInput op s
                                  }

-- | A set of operations that fail for every operation involving
-- non-default memory spaces.  Uses plain pointers and @malloc@ for
-- memory management.
defaultOperations :: Operations op s
defaultOperations = Operations { opsWriteScalar = defWriteScalar
                               , opsReadScalar = defReadScalar
                               , opsAllocate  = defAllocate
                               , opsCopy = defCopy
                               , opsStaticArray = defStaticArray
                               , opsCompiler = defCompiler
                               , opsEntryOutput = defEntryOutput
                               , opsEntryInput = defEntryInput
                               }
  where defWriteScalar _ _ _ _ _ =
          fail "Cannot write to non-default memory space because I am dumb"
        defReadScalar _ _ _ _ =
          fail "Cannot read from non-default memory space"
        defAllocate _ _ _ =
          fail "Cannot allocate in non-default memory space"
        defCopy _ _ _ _ _ _ _ _ =
          fail "Cannot copy to or from non-default memory space"
        defStaticArray _ _ _ _ =
          fail "Cannot create static array in non-default memory space"
        defCompiler _ =
          fail "The default compiler cannot compile extended operations"
        defEntryOutput _ _ _ _ =
          fail "Cannot return array not in default memory space"
        defEntryInput _ _ _ _ =
          fail "Cannot accept array not in default memory space"

data CompilerEnv op s = CompilerEnv {
    envOperations :: Operations op s
  , envFtable     :: M.Map Name [Imp.Type]
}

envOpCompiler :: CompilerEnv op s -> OpCompiler op s
envOpCompiler = opsCompiler . envOperations

envReadScalar :: CompilerEnv op s -> ReadScalar op s
envReadScalar = opsReadScalar . envOperations

envWriteScalar :: CompilerEnv op s -> WriteScalar op s
envWriteScalar = opsWriteScalar . envOperations

envAllocate :: CompilerEnv op s -> Allocate op s
envAllocate = opsAllocate . envOperations

envCopy :: CompilerEnv op s -> Copy op s
envCopy = opsCopy . envOperations

envStaticArray :: CompilerEnv op s -> StaticArray op s
envStaticArray = opsStaticArray . envOperations

envEntryOutput :: CompilerEnv op s -> EntryOutput op s
envEntryOutput = opsEntryOutput . envOperations

envEntryInput :: CompilerEnv op s -> EntryInput op s
envEntryInput = opsEntryInput . envOperations

newCompilerEnv :: Imp.Functions op -> Operations op s -> CompilerEnv op s
newCompilerEnv (Imp.Functions funs) ops =
  CompilerEnv { envOperations = ops
              , envFtable = ftable <> builtinFtable
              }
  where ftable = M.fromList $ map funReturn funs
        funReturn (name, Imp.Function _ outparams _ _ _ _) = (name, paramsTypes outparams)
        builtinFtable = M.map (map Imp.Scalar . snd) builtInFunctions

data CompilerState s = CompilerState {
    compNameSrc :: VNameSource
  , compBeforeParse :: [CSStmt]
  , compInit :: [CSStmt]
  , compDebugItems :: [CSStmt]
  , compUserState :: s
  , compMemberDecls :: [CSStmt]
}

newCompilerState :: VNameSource -> s -> CompilerState s
newCompilerState src s = CompilerState { compNameSrc = src
                                       , compBeforeParse = []
                                       , compInit = []
                                       , compDebugItems = []
                                       , compMemberDecls = []
                                       , compUserState = s }

newtype CompilerM op s a = CompilerM (RWS (CompilerEnv op s) [CSStmt] (CompilerState s) a)
  deriving (Functor, Applicative, Monad,
            MonadState (CompilerState s),
            MonadReader (CompilerEnv op s),
            MonadWriter [CSStmt])

instance MonadFreshNames (CompilerM op s) where
  getNameSource = gets compNameSrc
  putNameSource src = modify $ \s -> s { compNameSrc = src }
collect :: CompilerM op s () -> CompilerM op s [CSStmt]
collect m = pass $ do
  ((), w) <- listen m
  return (w, const mempty)

collect' :: CompilerM op s a -> CompilerM op s (a, [CSStmt])
collect' m = pass $ do
  (x, w) <- listen m
  return ((x, w), const mempty)

beforeParse :: CSStmt -> CompilerM op s ()
beforeParse x = modify $ \s ->
  s { compBeforeParse = compBeforeParse s ++ [x] }

atInit :: CSStmt -> CompilerM op s ()
atInit x = modify $ \s ->
  s { compInit = compInit s ++ [x] }

addMemberDecl :: CSStmt -> CompilerM op s ()
addMemberDecl x = modify $ \s ->
  s { compMemberDecls = compMemberDecls s ++ [x] }

contextFinalInits :: CompilerM op s [CSStmt]
contextFinalInits = gets compInit

stm :: CSStmt -> CompilerM op s ()
stm x = tell [x]

stms :: [CSStmt] -> CompilerM op s ()
stms = mapM_ stm

debugReport :: CSStmt -> CompilerM op s ()
debugReport x = modify $ \s ->
  s { compDebugItems = compDebugItems s ++ [x] }

futharkFun :: String -> String
futharkFun s = "futhark_" ++ zEncodeString s

paramsTypes :: [Imp.Param] -> [Imp.Type]
paramsTypes = map paramType

paramType :: Imp.Param -> Imp.Type
paramType (Imp.MemParam _ space) = Imp.Mem (Imp.ConstSize 0) space
paramType (Imp.ScalarParam _ t) = Imp.Scalar t

compileOutput :: Imp.Param -> (CSExp, CSType)
compileOutput = nameFun &&& typeFun
  where nameFun = Var . compileName . Imp.paramName
        typeFun = compileType . paramType

getDefaultDecl :: Imp.Param -> CSStmt
getDefaultDecl (Imp.MemParam v DefaultSpace) =
  Assign (Var $ compileName v) $ simpleCall "allocateMem" [Integer 0]
getDefaultDecl (Imp.MemParam v _) =
  AssignTyped (CustomT "CLMemoryHandle") (Var $ compileName v) (Just $ Var "ctx.EMPTY_MEM_HANDLE")
getDefaultDecl (Imp.ScalarParam v Cert) =
  Assign (Var $ compileName v) $ Bool True
getDefaultDecl (Imp.ScalarParam v t) =
  Assign (Var $ compileName v) $ simpleInitClass (compilePrimType t) []


runCompilerM :: Imp.Functions op -> Operations op s
             -> VNameSource
             -> s
             -> CompilerM op s a
             -> a
runCompilerM prog ops src userstate (CompilerM m) =
  fst $ evalRWS m (newCompilerEnv prog ops) (newCompilerState src userstate)

standardOptions :: [Option]
standardOptions = [
  Option { optionLongName = "write-runtime-to"
         , optionShortName = Just 't'
         , optionArgument = RequiredArgument
         , optionAction =
           [
             If (BinOp "!=" (Var "runtime_file") Null)
             [Exp $ simpleCall "runtime_file.Close" []] []
           , Reassign (Var "runtime_file") $
             simpleInitClass "FileStream" [Var "optarg", Var "FileMode.Create"]
           , Reassign (Var "runtime_file_writer") $
             simpleInitClass "StreamWriter" [Var "runtime_file"]
           ]
         },
  Option { optionLongName = "runs"
         , optionShortName = Just 'r'
         , optionArgument = RequiredArgument
         , optionAction =
           [ Reassign (Var "num_runs") $ simpleCall "Convert.ToInt32" [Var "optarg"]
           , Reassign (Var "do_warmup_run") $ Bool True
           ]
         },
  Option { optionLongName = "entry-point"
         , optionShortName = Just 'e'
         , optionArgument = RequiredArgument
         , optionAction =
           [ Reassign (Var "entry_point") $ Var "optarg" ]
         }
  ]

-- | The class generated by the code generator must have a
-- constructor, although it can be vacuous.
data Constructor = Constructor [CSFunDefArg] [CSStmt]

-- | A constructor that takes no arguments and does nothing.
emptyConstructor :: Constructor
emptyConstructor = Constructor [(Composite $ ArrayT $ Primitive StringT, "args")] []

constructorToConstructorDef :: Constructor -> String -> [CSStmt] -> CSStmt
constructorToConstructorDef (Constructor params body) name at_init =
  ConstructorDef $ ClassConstructor name params $ body <> at_init


compileProg :: MonadFreshNames m =>
               Maybe String
            -> Constructor
            -> [CSStmt]
            -> [CSStmt]
            -> Operations op s
            -> s
            -> CompilerM op s ()
            -> [CSStmt]
            -> [Space]
            -> [Option]
            -> Imp.Functions op
            -> m String
compileProg module_name constructor imports defines ops userstate boilerplate pre_timing spaces options prog@(Imp.Functions funs) = do
  src <- getNameSource
  let prog' = runCompilerM prog ops src userstate compileProg'
  let imports' = [ Using Nothing "System"
                 , Using Nothing "System.Diagnostics"
                 , Using Nothing "System.Collections"
                 , Using Nothing "System.Collections.Generic"
                 , Using Nothing "System.IO"
                 , Using Nothing "System.Linq"
                 , Using Nothing "static System.ValueTuple"
                 , Using Nothing "static System.Convert"
                 , Using Nothing "static System.Math"
                 , Using Nothing "Cloo"
                 , Using Nothing "Cloo.Bindings"
                 , Using Nothing "Mono.Options" ] ++ imports

  return $ pretty (CSProg $ Escape "#define DEBUG" : imports' ++ prog')
  where compileProg' = do
          definitions <- mapM compileFunc funs
          opencl_boilerplate <- collect boilerplate
          compBeforeParses <- gets compBeforeParse
          compInits <- gets compInit
          extraMemberDecls <- gets compMemberDecls
          let member_decls' = member_decls ++ extraMemberDecls
          let at_inits' = at_inits ++ compBeforeParses ++ parse_options ++ compInits


          case module_name of
            Just name -> do
              entry_points <- mapM compileEntryFun $ filter (Imp.functionEntry . snd) funs
              let constructor' = constructorToConstructorDef constructor name at_inits'
              return [ClassDef $ Class name $ constructor' : defines' ++ opencl_boilerplate ++
                      map FunDef (definitions ++ entry_points)]


            Nothing -> do
              let name = "FutharkInternal"
              let constructor' = constructorToConstructorDef constructor name at_inits'
              (entry_point_defs, entry_point_names, entry_points) <-
                unzip3 <$> mapM (callEntryFun pre_timing)
                (filter (Imp.functionEntry . snd) funs)

              debug_ending <- gets compDebugItems
              return ((ClassDef $
                       Class name $
                         member_decls' ++
                         constructor' : defines' ++
                         opencl_boilerplate ++
                         map FunDef (definitions ++ entry_point_defs) ++
                         [PublicFunDef $ Def "internal_entry" VoidT [] $ selectEntryPoint entry_point_names entry_points ++ debug_ending
                         ]
                      ) :
                     [ClassDef $ Class "Program"
                       [StaticFunDef $ Def "Main" VoidT [(string_arrayT,"args")] main_entry]])


        string_arrayT = Composite $ ArrayT $ Primitive StringT
        main_entry :: [CSStmt]
        main_entry = [ Assign (Var "internal_instance") (simpleInitClass "FutharkInternal" [Var "args"])
                     , Exp $ simpleCall "internal_instance.internal_entry" []
                     ]

        member_decls =
          [ AssignTyped (CustomT "FileStream") (Var "runtime_file") Nothing
          , AssignTyped (CustomT "StreamWriter") (Var "runtime_file_writer") Nothing
          , AssignTyped (Primitive BoolT) (Var "do_warmup_run") Nothing
          , AssignTyped (Primitive $ CSInt Int32T) (Var "num_runs") Nothing
          , AssignTyped (Primitive StringT) (Var "entry_point") Nothing
          ]

        at_inits = [ Reassign (Var "do_warmup_run") (Bool False)
                   , Reassign (Var "num_runs") (Integer 1)
                   , Reassign (Var "entry_point") (String "main")
                   ]

        defines' = [ Escape csScalar
                   , Escape csMemory
                   , Escape csExceptions
                   , Escape csReader] ++ defines

        parse_options =
          generateOptionParser (standardOptions ++ options)

        selectEntryPoint entry_point_names entry_points =
          [ Assign (Var "entry_points") $
              Collection "Dictionary<string, Action>" $ zipWith Pair (map String entry_point_names) entry_points,
            If (simpleCall "!entry_points.ContainsKey" [Var "entry_point"])
              [ Exp $ simpleCall "Console.WriteLine"
                  [simpleCall "string.Format"
                    [ String "No entry point '{}'.  Select another with --entry point.  Options are:\n{}"
                    , Var "entry_point"
                    , simpleCall "string.Join"
                        [ String "\n"
                        , Field (Var "entry_points") "Keys" ]]]
              , Exp $ simpleCall "Environment.Exit" [Integer 1]]
              [ Assign (Var "entry_point_fun") $
                  Index (Var "entry_points") (IdxExp $ Var "entry_point")
              , Exp $ simpleCall "entry_point_fun.Invoke" []]
          ]


compileFunc :: (Name, Imp.Function op) -> CompilerM op s CSFunDef
compileFunc (fname, Imp.Function _ outputs inputs body _ _) = do
  body' <- collect $ compileCode body
  let inputs' = map compileTypedInput inputs
  let outputs' = map compileOutput outputs
  let outputDecls = map getDefaultDecl outputs
  let (ret, retType) = unzip outputs'
  let retType' = tupleOrSingleT retType
  let ret' = [Return $ tupleOrSingle ret]

  case outputs of
    [] -> return $ Def (futharkFun . nameToString $ fname) VoidT inputs' (outputDecls++body')
    _ -> return $ Def (futharkFun . nameToString $ fname) retType' inputs' (outputDecls++body'++ret')


compileTypedInput :: Imp.Param -> (CSType, String)
compileTypedInput input = (typeFun input, nameFun input)
  where nameFun = compileName . Imp.paramName
        typeFun = compileType . paramType

tupleOrSingle :: [CSExp] -> CSExp
tupleOrSingle [e] = e
tupleOrSingle es = Tuple es

tupleOrSingleT :: [CSType] -> CSType
tupleOrSingleT [e] = e
tupleOrSingleT es = Composite $ TupleT es

assignArrayPointer :: CSExp -> CSExp -> CSStmt
assignArrayPointer e ptr =
  AssignTyped (PointerT VoidT) ptr (Just $ Addr $ Index e (IdxExp $ Integer 0))

assignScalarPointer :: CSExp -> CSExp -> CSStmt
assignScalarPointer e ptr =
  AssignTyped (PointerT VoidT) ptr (Just $ Addr e)

-- | A 'Call' where the function is a variable and every argument is a
-- simple 'Arg'.
simpleCall :: String -> [CSExp] -> CSExp
simpleCall fname = Call (Var fname) . map simpleArg

-- | A 'Call' where the function is a variable and every argument is a
-- simple 'Arg'.
parametrizedCall :: String -> String -> [CSExp] -> CSExp
parametrizedCall fname primtype = Call (Var fname') . map simpleArg
  where fname' = concat [fname, "<", primtype, ">"]

simpleArg :: CSExp -> CSArg
simpleArg = Arg Nothing

-- | A CallMethod
callMethod :: CSExp -> String -> [CSExp] -> CSExp
callMethod object method = CallMethod object (Var method) . map simpleArg

simpleInitClass :: String -> [CSExp] -> CSExp
simpleInitClass fname = CreateObject (Var fname) . map simpleArg

compileName :: VName -> String
compileName = zEncodeString . pretty

compileType :: Imp.Type -> CSType
compileType (Imp.Scalar p) = compilePrimTypeToAST p
compileType (Imp.Mem _ space) = rawMemCSType space

compileSpace :: Imp.Space -> String
compileSpace _ = "placehodler"

compilePrimTypeToAST :: PrimType -> CSType
compilePrimTypeToAST (IntType Int8) = Primitive $ CSInt Int8T
compilePrimTypeToAST (IntType Int16) = Primitive $ CSInt Int16T
compilePrimTypeToAST (IntType Int32) = Primitive $ CSInt Int32T
compilePrimTypeToAST (IntType Int64) = Primitive $ CSInt Int64T
compilePrimTypeToAST (FloatType Float32) = Primitive $ CSFloat FloatT
compilePrimTypeToAST (FloatType Float64) = Primitive $ CSFloat DoubleT
compilePrimTypeToAST Imp.Bool = Primitive BoolT
compilePrimTypeToAST Imp.Cert = Primitive BoolT

compilePrimTypeToASText :: PrimType -> Imp.Signedness -> CSType
compilePrimTypeToASText (IntType Int8) Imp.TypeUnsigned = Primitive  $ CSUInt UInt8T
compilePrimTypeToASText (IntType Int16) Imp.TypeUnsigned = Primitive $ CSUInt UInt16T
compilePrimTypeToASText (IntType Int32) Imp.TypeUnsigned = Primitive $ CSUInt UInt32T
compilePrimTypeToASText (IntType Int64) Imp.TypeUnsigned = Primitive $ CSUInt UInt64T
compilePrimTypeToASText (IntType Int8) _ = Primitive $ CSInt Int8T
compilePrimTypeToASText (IntType Int16) _ = Primitive $ CSInt Int16T
compilePrimTypeToASText (IntType Int32) _ = Primitive $ CSInt Int32T
compilePrimTypeToASText (IntType Int64) _ = Primitive $ CSInt Int64T
compilePrimTypeToASText (FloatType Float32) _ = Primitive $ CSFloat FloatT
compilePrimTypeToASText (FloatType Float64) _ = Primitive $ CSFloat DoubleT
compilePrimTypeToASText Imp.Bool _ = Primitive BoolT
compilePrimTypeToASText Imp.Cert _ = Primitive BoolT

compileDim :: Imp.DimSize -> CSExp
compileDim (Imp.ConstSize i) = Integer $ toInteger i
compileDim (Imp.VarSize v) = Var $ compileName v

unpackDim :: CSExp -> Imp.DimSize -> Int32 -> CompilerM op s ()
unpackDim arr_name (Imp.ConstSize c) i = do
  let shape_name = Field arr_name "shape"
  let constant_c = Integer $ toInteger c
  let constant_i = Integer $ toInteger i
  stm $ Assert (BinOp "==" constant_c (Index shape_name $ IdxExp constant_i)) "constant dimension wrong"

unpackDim arr_name (Imp.VarSize var) i = do
  let shape_name = Field arr_name "shape"
  let src = Index shape_name $ IdxExp $ Integer $ toInteger i
  let dest = Var $ compileName var
  stm $ Assign dest src

entryPointOutput :: Imp.ExternalValue -> CompilerM op s CSExp
entryPointOutput (Imp.OpaqueValue desc vs) =
  simpleInitClass "Opaque" . (String (pretty desc):) <$>
  mapM (entryPointOutput . Imp.TransparentValue) vs

entryPointOutput (Imp.TransparentValue (Imp.ScalarValue bt ept name)) =
  return $ simpleCall tf [Var $ compileName name]
  where tf = compileTypeConverterExt bt ept

entryPointOutput (Imp.TransparentValue (Imp.ArrayValue mem _ Imp.DefaultSpace bt ept dims)) = do
  let src = Var $ compileName mem
  let createArray = "createArray_" ++ compilePrimTypeExt bt ept
  return $ simpleCall createArray [src, CreateArray (Primitive $ CSInt Int64T) $ map compileDim dims]

entryPointOutput (Imp.TransparentValue (Imp.ArrayValue mem _ (Imp.Space sid) bt ept dims)) = do
  pack_output <- asks envEntryOutput
  pack_output mem sid bt ept dims

-- TODO
badInput :: Int -> CSExp -> String -> CSStmt
badInput i e t =
  Throw $ simpleInitClass "TypeError" [formatString err_msg [String t, e]]
  where err_msg = unlines [ "Argument #" ++ show i ++ " has invalid value"
                          , "Futhark type: {}"
                          , "Type of given C# value: {}" ]
getCSExpType :: CSExp -> CSExp
getCSExpType e = Field (callMethod e "GetType" []) "Name"

entryPointInput :: (Int, Imp.ExternalValue, CSExp) -> CompilerM op s ()
entryPointInput (i, Imp.OpaqueValue desc vs, e) = do
  let type_is_ok = BinOp "&&" (BinOp "is" e (Var "Opaque"))
                               (BinOp "==" (Field e "desc") (String desc))
  stm $ If (UnOp "!" type_is_ok) [badInput i (getCSExpType e) desc] []
  mapM_ entryPointInput $ zip3 (repeat i) (map Imp.TransparentValue vs) $
    map (Index (Field e "data") . IdxExp . Integer) [0..]

entryPointInput (_, Imp.TransparentValue (Imp.ScalarValue bt _ name), e) = do
  let vname' = Var $ compileName name
      csconverter = compileTypeConverter bt
      cscall = simpleCall csconverter [e]
  stm $ Assign vname' cscall

entryPointInput (i, Imp.TransparentValue (Imp.ArrayValue mem memsize Imp.DefaultSpace t s dims), e) = do
  let type_is_wrong =
        UnOp "!" $
        BinOp "&&"
        (BinOp "is" e $ Var "FlatArray`1")
        (BinOp "==" (Field e "GetType().GetGenericArguments()[0]") (Var (compileSystemTypeExt t s)))
  stm $ If type_is_wrong
    [badInput i (getCSExpType e) $ concat (replicate (length dims) "[]") ++
     prettySigned (s==Imp.TypeUnsigned) t]
    []

  zipWithM_ (unpackDim e) dims [0..]
  let dest = Var $ compileName mem
      unwrap_call = simpleCall "unwrapArray" [e]

  case memsize of
    Imp.VarSize sizevar ->
      stm $ Assign (Var $ compileName sizevar) $ Field e "Length"
    Imp.ConstSize _ ->
      return ()

  stm $ Assign dest unwrap_call

entryPointInput (_, Imp.TransparentValue (Imp.ArrayValue mem memsize (Imp.Space sid) bt ept dims), e) = do
  stm $ Comment "supper" []
  unpack_input <- asks envEntryInput
  unpack <- collect $ unpack_input mem memsize sid bt ept dims e
  stms unpack

extValueDescName :: Imp.ExternalValue -> String
extValueDescName (Imp.TransparentValue v) = extName $ valueDescName v
extValueDescName (Imp.OpaqueValue desc []) = extName $ zEncodeString desc
extValueDescName (Imp.OpaqueValue desc (v:_)) =
  extName $ zEncodeString desc ++ "_" ++ pretty (baseTag (valueDescVName v))

extName :: String -> String
extName = (++"_ext")

sizeOf :: CSType -> CSExp
sizeOf t = simpleCall "sizeof" [(Var . pretty) t]

funDef :: String -> CSType -> [(CSType, String)] -> [CSStmt] -> CSStmt
funDef s t args stmts = FunDef $ Def s t args stmts

valueDescName :: Imp.ValueDesc -> String
valueDescName = compileName . valueDescVName

valueDescVName :: Imp.ValueDesc -> VName
valueDescVName (Imp.ScalarValue _ _ vname) = vname
valueDescVName (Imp.ArrayValue vname _ _ _ _ _) = vname

consoleWrite :: String -> [CSExp] -> CSExp
consoleWrite str exps = simpleCall "Console.Write" $ String str:exps

consoleWriteLine :: String -> [CSExp] -> CSExp
consoleWriteLine str exps = simpleCall "Console.WriteLine" $ String str:exps

consoleErrorWrite :: String -> [CSExp] -> CSExp
consoleErrorWrite str exps = simpleCall "Console.Error.Write" $ String str:exps

consoleErrorWriteLine :: String -> [CSExp] -> CSExp
consoleErrorWriteLine str exps = simpleCall "Console.Error.WriteLine" $ String str:exps

readFun :: PrimType -> Imp.Signedness -> String
readFun (FloatType Float32) _ = "read_f32"
readFun (FloatType Float64) _ = "read_f64"
readFun (IntType Int8)  Imp.TypeUnsigned = "read_u8"
readFun (IntType Int16) Imp.TypeUnsigned = "read_u16"
readFun (IntType Int32) Imp.TypeUnsigned = "read_u32"
readFun (IntType Int64) Imp.TypeUnsigned = "read_u64"
readFun (IntType Int8)  Imp.TypeDirect   = "read_i8"
readFun (IntType Int16) Imp.TypeDirect   = "read_i16"
readFun (IntType Int32) Imp.TypeDirect   = "read_i32"
readFun (IntType Int64) Imp.TypeDirect   = "read_i64"
readFun Imp.Bool _      = "read_bool"
readFun Cert _          = error "readFun: cert"

-- The value returned will be used when reading binary arrays, to indicate what
-- the expected type is
readTypeEnum :: PrimType -> Imp.Signedness -> String
readTypeEnum (IntType Int8)  Imp.TypeUnsigned = "FUTHARK_UINT8"
readTypeEnum (IntType Int16) Imp.TypeUnsigned = "FUTHARK_UINT16"
readTypeEnum (IntType Int32) Imp.TypeUnsigned = "FUTHARK_UINT32"
readTypeEnum (IntType Int64) Imp.TypeUnsigned = "FUTHARK_UINT64"
readTypeEnum (IntType Int8)  Imp.TypeDirect   = "FUTHARK_INT8"
readTypeEnum (IntType Int16) Imp.TypeDirect   = "FUTHARK_INT16"
readTypeEnum (IntType Int32) Imp.TypeDirect   = "FUTHARK_INT32"
readTypeEnum (IntType Int64) Imp.TypeDirect   = "FUTHARK_INT64"
readTypeEnum (FloatType Float32) _ = "FUTHARK_FLOAT32"
readTypeEnum (FloatType Float64) _ = "FUTHARK_FLOAT64"
readTypeEnum Imp.Bool _ = "FUTHARK_BOOL"
readTypeEnum Cert _ = error "readTypeEnum: cert"

readInput :: Imp.ExternalValue -> CSExp -> CSStmt
readInput (Imp.OpaqueValue desc _) _ =
  Throw $ simpleInitClass "Exception" [String $ "Cannot read argument of type " ++ desc ++ "."]

readInput decl@(Imp.TransparentValue (Imp.ScalarValue bt ept _)) stdin =
  let reader' = readFun bt ept
  in Assign (Var $ extValueDescName decl) $ simpleCall reader' [stdin]

-- TODO: If the type identifier of 'Float32' is changed, currently the error
-- messages for reading binary input will not use this new name. This is also a
-- problem for the C runtime system.
readInput decl@(Imp.TransparentValue (Imp.ArrayValue _ _ _ bt ept dims)) stdin =
  let rank' = Var $ show $ length dims
      type_enum = Var $ readTypeEnum bt ept
      ct = Var $ compileTypeConverterExt bt ept
  in Assign (Var $ extValueDescName decl) $ simpleCall "read_array"
     [stdin, type_enum, rank', ct]

printPrimStm :: CSExp -> PrimType -> Imp.Signedness -> CSStmt
printPrimStm val t ept =
  case (t, ept) of
    (IntType Int8, Imp.TypeUnsigned) -> p "{0}u8"
    (IntType Int16, Imp.TypeUnsigned) -> p "{0}u16"
    (IntType Int32, Imp.TypeUnsigned) -> p "{0}u32"
    (IntType Int64, Imp.TypeUnsigned) -> p "{0}u64"
    (IntType Int8, _) -> p "{0}i8"
    (IntType Int16, _) -> p "{0}i16"
    (IntType Int32, _) -> p "{0}i32"
    (IntType Int64, _) -> p "{0}i64"
    (Imp.Bool, _) -> If val
                     [Exp $ simpleCall "Console.Write" [String "true"]]
                     [Exp $ simpleCall "Console.Write" [String "false"]]
    (Cert, _) -> Exp $ simpleCall "Console.Write" [String "Checked"]
    (FloatType Float32, _) -> p "{0:0.000000}f32"
    (FloatType Float64, _) -> p "{0:0.000000}f64"
  where p s =
          Exp $ simpleCall "Console.Write" [formatString s [val]]

formatString :: String -> [CSExp] -> CSExp
formatString fmt contents =
  simpleCall "String.Format" $ String fmt : contents

printStm :: Imp.ValueDesc -> CSExp -> CompilerM op s CSStmt
printStm (Imp.ScalarValue bt ept _) e =
  return $ printPrimStm e bt ept
printStm (Imp.ArrayValue _ _ _ bt ept []) e =
  return $ printPrimStm e bt ept
printStm (Imp.ArrayValue mem memsize space bt ept (outer:shape)) e = do
  v <- newVName "print_elem"
  first <- newVName "print_first"
  let size = callMethod (CreateArray (Primitive $ CSInt Int32T) $ map compileDim $ outer:shape)
                 "Aggregate" [ Integer 1
                             , Lambda (Tuple [Var "acc", Var "val"])
                                      [Exp $ BinOp "*" (Var "acc") (Var "val")]
                             ]
      emptystr = "empty(" ++ ppArrayType bt (length shape) ++ ")"
  printelem <- printStm (Imp.ArrayValue mem memsize space bt ept shape) $ Var $ compileName v
  return $ If (BinOp "==" size (Integer 0))
    [puts emptystr]
    [Assign (Var $ pretty first) $ Var "true",
     puts "[",
     ForEach (pretty v) e [
        If (simpleCall "!" [Var $ pretty first])
        [puts ", "] [],
        printelem,
        Reassign (Var $ pretty first) $ Var "false"
    ],
    puts "]"]
    where ppArrayType :: PrimType -> Int -> String
          ppArrayType t 0 = prettyPrimType ept t
          ppArrayType t n = "[]" ++ ppArrayType t (n-1)

          prettyPrimType Imp.TypeUnsigned (IntType Int8) = "u8"
          prettyPrimType Imp.TypeUnsigned (IntType Int16) = "u16"
          prettyPrimType Imp.TypeUnsigned (IntType Int32) = "u32"
          prettyPrimType Imp.TypeUnsigned (IntType Int64) = "u64"
          prettyPrimType _ t = pretty t

          puts s = Exp $ simpleCall "Console.Write" [String s]

printValue :: [(Imp.ExternalValue, CSExp)] -> CompilerM op s [CSStmt]
printValue = fmap concat . mapM (uncurry printValue')
  -- We copy non-host arrays to the host before printing.  This is
  -- done in a hacky way - we assume the value has a .get()-method
  -- that returns an equivalent Numpy array.  This works for CSOpenCL,
  -- but we will probably need yet another plugin mechanism here in
  -- the future.
  where printValue' (Imp.OpaqueValue desc _) _ =
          return [Exp $ simpleCall "Console.Write"
                  [String $ "#<opaque " ++ desc ++ ">"]]
        printValue' (Imp.TransparentValue r) e = do
          p <- printStm r e
          return [p, Exp $ simpleCall "Console.Write" [String "\n"]]

prepareEntry :: (Name, Imp.Function op) -> CompilerM op s
                (String, [(CSType, String)], CSType, [CSStmt], [CSStmt], [CSStmt], [CSStmt],
                 [(Imp.ExternalValue, CSExp)], [CSStmt])
prepareEntry (fname, Imp.Function _ outputs inputs _ results args) = do
  let (output_types, output_paramNames) = unzip $ map compileTypedInput outputs
      funTuple = tupleOrSingle $ fmap Var output_paramNames

  (argexps_mem_copies, prepare_run) <- collect' $ forM inputs $ \case
    Imp.MemParam name space -> do
      -- A program might write to its input parameters, so create a new memory
      -- block and copy the source there.  This way the program can be run more
      -- than once.
      name' <- newVName $ baseString name <> "_copy"
      copy <- asks envCopy
      allocate <- asks envAllocate
      let size = Var (extName (compileName name) ++ ".Length") -- FIXME
          dest = name'
          src = name
          offset = Integer 0
      case space of
        DefaultSpace ->
          stm $ Assign (Var (compileName name'))
                       (simpleCall "allocateMem" [size]) -- FIXME
        Space sid ->
          allocate name' size sid
      copy dest offset space src offset space size (IntType Int64) -- FIXME
      return $ Just $ compileName name'
    _ -> return Nothing

  prepareIn <- collect $ mapM_ entryPointInput $ zip3 [0..] args $
               map (Var . extValueDescName) args
  (res, prepareOut) <- collect' $ mapM entryPointOutput results

  let argexps_lib = map (compileName . Imp.paramName) inputs
      argexps_bin = zipWith fromMaybe argexps_lib argexps_mem_copies
      fname' = futharkFun (nameToString fname)
      arg_types = map (fst . compileTypedInput) inputs
      inputs' = zip arg_types (map extValueDescName args)
      output_type = tupleOrSingleT output_types
      call_lib = [Reassign funTuple $ simpleCall fname' (fmap Var argexps_lib)]
      call_bin = [Reassign funTuple $ simpleCall fname' (fmap Var argexps_bin)]

  return (nameToString fname, inputs', output_type,
          prepareIn, call_lib, call_bin, prepareOut,
          zip results res, prepare_run)


copyMemoryDefaultSpace :: VName -> CSExp -> VName -> CSExp -> CSExp ->
                          CompilerM op s ()
copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes =
  stm $ Exp $ simpleCall "Buffer.BlockCopy" [ Var (compileName srcmem), srcidx
                                      , Var (compileName destmem), destidx,
                                        nbytes]

compileEntryFun :: (Name, Imp.Function op)
                -> CompilerM op s CSFunDef
compileEntryFun entry = do
  (fname', params, outputType, prepareIn, body_lib, _, prepareOut, res, _) <- prepareEntry entry
  let ret = Return $ tupleOrSingle $ map snd res
  return $ Def fname' outputType params $
    prepareIn ++ body_lib ++ prepareOut ++ [ret]


callEntryFun :: [CSStmt] -> (Name, Imp.Function op)
             -> CompilerM op s (CSFunDef, String, CSExp)
callEntryFun pre_timing entry@(fname, Imp.Function _ outputs _ _ _ decl_args) = do
  (_, _, outputType, prepareIn, _, body_bin, _, res, prepare_run) <- prepareEntry entry

  let stdin = Var "stdin_stream"
  let get_reader = [Assign stdin $ simpleCall "getStream" []]
  let str_input = map (`readInput` stdin) decl_args
  let outputDecls = map getDefaultDecl outputs
      exitcall = [
          Exp $ simpleCall "Console.WriteLine" [formatString "Assertion.{} failed" [Var "e"]]
        , Exp $ simpleCall "Environment.Exit" [Integer 1]
        ]
      except' = Catch (Var "Exception") exitcall
      do_run = body_bin ++ pre_timing
      (do_run_with_timing, close_runtime_file) = addTiming do_run

      -- We ignore overflow errors and the like for executable entry
      -- points.  These are (somewhat) well-defined in Futhark.

      do_warmup_run =
        If (Var "do_warmup_run") (prepare_run ++ do_run) []

      do_num_runs =
        For "i" (Var "num_runs") (prepare_run ++ do_run_with_timing)

  str_output <- printValue res

  let fname' = "entry_" ++ nameToString fname

  return (Def fname' VoidT [] $
           get_reader ++ str_input ++ prepareIn ++ outputDecls ++
           [Try [do_warmup_run, do_num_runs] [except']] ++
           [close_runtime_file] ++
           str_output,

          nameToString fname,

          Var fname')


addTiming :: [CSStmt] -> ([CSStmt], CSStmt)
addTiming statements =
  ([ Assign (Var "stop_watch") $ simpleInitClass "Stopwatch" []
   , Exp $ simpleCall "stop_watch.Start" [] ] ++
   statements ++
   [ Exp $ simpleCall "stop_watch.Stop" []
   , Assign (Var "time_elapsed") $ asMicroseconds (Var "stop_watch")
   , If (not_null (Var "runtime_file")) print_runtime []
   ]
   , If (not_null (Var "runtime_file")) [Exp $ simpleCall "runtime_file.Close" []] []
  )

  where print_runtime =
          [Exp $ simpleCall "runtime_file_writer.WriteLine"
           [ callMethod (Var "time_elapsed") "ToString" [] ],
           Exp $ simpleCall "runtime_file_writer.WriteLine" [String "\n"]]

        not_null var = BinOp "!=" var Null

        asMicroseconds watch =
          BinOp "/" (Field watch "ElapsedTicks")
          (BinOp "/" (Field (Var "TimeSpan") "TicksPerMillisecond") (Integer 1000))

compileUnOp :: Imp.UnOp -> String
compileUnOp op =
  case op of
    Not -> "!"
    Complement{} -> "~"
    Abs{} -> "Math.Abs" -- actually write these helpers
    FAbs{} -> "Math.Abs"
    SSignum{} -> "ssignum"
    USignum{} -> "usignum"

compileBinOpLike :: Monad m =>
                    Imp.Exp -> Imp.Exp
                 -> CompilerM op s (CSExp, CSExp, String -> m CSExp)
compileBinOpLike x y = do
  x' <- compileExp x
  y' <- compileExp y
  let simple s = return $ BinOp s x' y'
  return (x', y', simple)

-- | The ctypes type corresponding to a 'PrimType'.
compilePrimType :: PrimType -> String
compilePrimType t =
  case t of
    IntType Int8 -> "byte"
    IntType Int16 -> "short"
    IntType Int32 -> "int"
    IntType Int64 -> "long"
    FloatType Float32 -> "float"
    FloatType Float64 -> "double"
    Imp.Bool -> "bool"
    Cert -> "bool"

-- | The ctypes type corresponding to a 'PrimType', taking sign into account.
compilePrimTypeExt :: PrimType -> Imp.Signedness -> String
compilePrimTypeExt t ept =
  case (t, ept) of
    (IntType Int8, Imp.TypeUnsigned) -> "byte"
    (IntType Int16, Imp.TypeUnsigned) -> "ushort"
    (IntType Int32, Imp.TypeUnsigned) -> "uint"
    (IntType Int64, Imp.TypeUnsigned) -> "ulong"
    (IntType Int8, _) -> "sbyte"
    (IntType Int16, _) -> "short"
    (IntType Int32, _) -> "int"
    (IntType Int64, _) -> "long"
    (FloatType Float32, _) -> "float"
    (FloatType Float64, _) -> "double"
    (Imp.Bool, _) -> "bool"
    (Cert, _) -> "byte"

-- | Select function to retrieve bytes from byte array as specific data type
compileBitConverter :: PrimType -> String
compileBitConverter t =
  case t of
    IntType Int8 -> "BitConverter.ToInt8"
    IntType Int16 -> "BitConverter.ToInt16"
    IntType Int32 -> "BitConverter.ToInt32"
    IntType Int64 -> "BitConverter.ToInt64"
    FloatType Float32 -> "BitConverter.ToSingle"
    FloatType Float64 -> "BitConverter.ToDouble"
    Imp.Bool -> "BitConverter.ToBoolean"
    Cert -> "BitConverter.ToBoolean"

compileBitConverterExt :: PrimType ->  Imp.Signedness -> String
compileBitConverterExt t ept =
  case (t, ept) of
    (IntType Int8, Imp.TypeUnsigned) -> "BitConverter.ToUInt8"
    (IntType Int16, Imp.TypeUnsigned) -> "BitConverter.ToUInt16"
    (IntType Int32, Imp.TypeUnsigned) -> "BitConverter.ToUInt32"
    (IntType Int64, Imp.TypeUnsigned) -> "BitConverter.ToUInt64"
    (IntType Int8, _) -> "BitConverter.ToInt8"
    (IntType Int16, _) -> "BitConverter.ToInt16"
    (IntType Int32, _) -> "BitConverter.ToInt32"
    (IntType Int64, _) -> "BitConverter.ToInt64"
    (FloatType Float32, _) -> "BitConverter.ToSingle"
    (FloatType Float64, _) -> "BitConverter.ToDouble"
    (Imp.Bool, _) -> "BitConverter.ToBoolean"
    (Cert, _) -> "BitConverter.ToBoolean"

-- | The ctypes type corresponding to a 'PrimType'.
compileTypeConverter :: PrimType -> String
compileTypeConverter t =
  case t of
    IntType Int8 -> "Convert.ToByte"
    IntType Int16 -> "Convert.ToInt16"
    IntType Int32 -> "Convert.ToInt32"
    IntType Int64 -> "Convert.ToInt64"
    FloatType Float32 -> "Convert.ToSingle"
    FloatType Float64 -> "Convert.ToDouble"
    Imp.Bool -> "Convert.ToBool"
    Cert -> "Convert.ToByte"

-- | The ctypes type corresponding to a 'PrimType', taking sign into account.
compileTypeConverterExt :: PrimType -> Imp.Signedness -> String
compileTypeConverterExt t ept =
  case (t, ept) of
    (IntType Int8, Imp.TypeUnsigned) -> "Convert.ToByte"
    (IntType Int16, Imp.TypeUnsigned) -> "Convert.ToUInt16"
    (IntType Int32, Imp.TypeUnsigned) -> "Convert.ToUInt32"
    (IntType Int64, Imp.TypeUnsigned) -> "Convert.ToUInt64"
    (IntType Int8 , _) -> "Convert.ToSByte"
    (IntType Int16 , _) -> "Convert.ToInt16"
    (IntType Int32 , _) -> "Convert.ToInt32"
    (IntType Int64 , _) -> "Convert.ToInt64"
    (FloatType Float32 , _) -> "Convert.ToSingle"
    (FloatType Float64 , _) -> "Convert.ToDouble"
    (Imp.Bool , _) -> "Convert.ToBool"
    (Imp.Cert , _) -> "Convert.ToBool"

-- | The ctypes type corresponding to a 'PrimType', taking sign into account.
compileSystemTypeExt :: PrimType -> Imp.Signedness -> String
compileSystemTypeExt t ept =
  case (t, ept) of
    (IntType Int8, Imp.TypeUnsigned) -> "System.Byte"
    (IntType Int16, Imp.TypeUnsigned) -> "System.UInt16"
    (IntType Int32, Imp.TypeUnsigned) -> "System.UInt32"
    (IntType Int64, Imp.TypeUnsigned) -> "System.UInt64"
    (IntType Int8 , _) -> "System.SByte"
    (IntType Int16 , _) -> "System.Int16"
    (IntType Int32 , _) -> "System.Int32"
    (IntType Int64 , _) -> "System.Int64"
    (FloatType Float32 , _) -> "System.Single"
    (FloatType Float64 , _) -> "System.Double"
    (Imp.Bool , _) -> "System.Boolean"
    (Imp.Cert , _) -> "System.Boolean"



compilePrimValue :: Imp.PrimValue -> CSExp
compilePrimValue (IntValue (Int8Value v)) =
  simpleCall "Convert.ToByte" [Integer $ toInteger v]
compilePrimValue (IntValue (Int16Value v)) =
  simpleCall "Convert.ToInt16" [Integer $ toInteger v]
compilePrimValue (IntValue (Int32Value v)) =
  simpleCall "Convert.ToInt32" [Integer $ toInteger v]
compilePrimValue (IntValue (Int64Value v)) =
  simpleCall "Convert.ToInt64" [Integer $ toInteger v]
compilePrimValue (FloatValue (Float32Value v))
  | isInfinite v =
      if v > 0 then Var "Single.PositiveInfinity" else Var "Single.NegativeInfinity"
  | isNaN v =
      Var "Single.NaN"
  | otherwise = simpleCall "Convert.ToSingle" [Float $ fromRational $ toRational v]
compilePrimValue (FloatValue (Float64Value v))
  | isInfinite v =
      if v > 0 then Var "Double.PositiveInfinity" else Var "Double.NegativeInfinity"
  | isNaN v =
      Var "Double.NaN"
  | otherwise = simpleCall "Convert.ToDouble" [Float $ fromRational $ toRational v]
compilePrimValue (BoolValue v) = Bool v
compilePrimValue Checked = Bool True

compileExp :: Imp.Exp -> CompilerM op s CSExp

compileExp (Imp.ValueExp v) = return $ compilePrimValue v

compileExp (Imp.LeafExp (Imp.ScalarVar vname) _) =
  return $ Var $ compileName vname

compileExp (Imp.LeafExp (Imp.SizeOf t) _) =
  return $ simpleCall (compileTypeConverter $ IntType Int32) [Integer $ primByteSize t]

compileExp (Imp.LeafExp (Imp.Index src (Imp.Count iexp) bt DefaultSpace _) _) = do
  iexp' <- compileExp iexp
  let bt' = compilePrimType bt
  let converter = compileBitConverter bt
  return $ parametrizedCall "indexArray" bt' [Var $ compileName src, iexp', Var converter]

compileExp (Imp.LeafExp (Imp.Index src (Imp.Count iexp) restype (Imp.Space space) _) _) =
  join $ asks envReadScalar
    <*> pure src <*> compileExp iexp
    <*> pure restype <*> pure space

compileExp (Imp.BinOpExp op x y) = do
  (x', y', simple) <- compileBinOpLike x y
  case op of
    Add{} -> simple "+"
    Sub{} -> simple "-"
    Mul{} -> simple "*"
    FAdd{} -> simple "+"
    FSub{} -> simple "-"
    FMul{} -> simple "*"
    FDiv{} -> simple "/"
    Xor{} -> simple "^"
    And{} -> simple "&"
    Or{} -> simple "|"
    Shl{} -> simple "<<"
    LogAnd{} -> simple "&&"
    LogOr{} -> simple "||"
    _ -> return $ simpleCall (pretty op) [x', y']

compileExp (Imp.ConvOpExp conv x) = do
  x' <- compileExp x
  return $ simpleCall (pretty conv) [x']

compileExp (Imp.CmpOpExp cmp x y) = do
  (x', y', simple) <- compileBinOpLike x y
  case cmp of
    CmpEq{} -> simple "=="
    FCmpLt{} -> simple "<"
    FCmpLe{} -> simple "<="
    _ -> return $ simpleCall (pretty cmp) [x', y']

compileExp (Imp.UnOpExp op exp1) = do
  exp1' <- compileExp exp1
  return $ UnOp (compileUnOp op) exp1'

compileExp (Imp.FunExp h args _) =
  simpleCall (futharkFun (pretty h)) <$> mapM compileExp args

compileCode :: Imp.Code op -> CompilerM op s ()

compileCode Imp.DebugPrint{} =
  return ()

compileCode (Imp.Op op) =
  join $ asks envOpCompiler <*> pure op

compileCode (Imp.If cond tb fb) = do
  cond' <- compileExp cond
  tb' <- collect $ compileCode tb
  fb' <- collect $ compileCode fb
  stm $ If cond' tb' fb'

compileCode (c1 Imp.:>>: c2) = do
  compileCode c1
  compileCode c2

compileCode (Imp.While cond body) = do
  cond' <- compileExp cond
  body' <- collect $ compileCode body
  stm $ While cond' body'

compileCode (Imp.For i it bound body) = do
  bound' <- compileExp bound
  let i' = compileName i
  body' <- collect $ compileCode body
  counter <- pretty <$> newVName "counter"
  one <- pretty <$> newVName "one"
  stm $ Assign (Var i') $ simpleCall (compileTypeConverter (IntType it)) [Integer 0]
  stm $ Assign (Var one) $ simpleCall (compileTypeConverter (IntType it)) [Integer 1]
  stm $ For counter bound' $ body' ++
    [AssignOp "+" (Var i') (Var one)]


compileCode (Imp.SetScalar vname exp1) = do
  let name' = Var $ compileName vname
  exp1' <- compileExp exp1
  stm $ Reassign name' exp1'

compileCode (Imp.DeclareMem v space) = declMem v space

compileCode (Imp.DeclareScalar v Cert) =
  stm $ Assign (Var $ compileName v) $ Bool True
compileCode (Imp.DeclareScalar v t) =
  stm $ AssignTyped t' (Var $ compileName v) Nothing
  where t' = compilePrimTypeToAST t

compileCode (Imp.DeclareArray name DefaultSpace t vs) = do
  atInit $ Assign (Var $ "init_"++name') $
    simpleCall "unwrapArray"
    [
      CreateArray (compilePrimTypeToAST t) (map compilePrimValue vs)
    , simpleCall "sizeof" [Var $ compilePrimType t]
    ]
  stm $ Assign (Var name') $ Var ("init_"++name')
  where name' = compileName name


compileCode (Imp.DeclareArray name (Space space) t vs) =
  join $ asks envStaticArray <*>
  pure name <*> pure space <*> pure t <*> pure vs

compileCode (Imp.Comment s code) = do
  code' <- collect $ compileCode code
  stm $ Comment s code'

compileCode (Imp.Assert e msg (loc,locs)) = do
  e' <- compileExp e
  stm $ Assert e' ("At " ++ stacktrace ++ ": " ++ msg)
  where stacktrace = intercalate " -> " (reverse $ map locStr $ loc:locs)

compileCode (Imp.Call dests fname args) = do
  args' <- mapM compileArg args
  let dests' = tupleOrSingle $ fmap Var (map compileName dests)
      fname' = futharkFun (pretty fname)
      call' = simpleCall fname' args'
  -- If the function returns nothing (is called only for side
  -- effects), take care not to assign to an empty tuple.
  stm $ if null dests
        then Exp call'
        else Reassign dests' call'
  where compileArg (Imp.MemArg m) = return $ Var $ compileName m
        compileArg (Imp.ExpArg e) = compileExp e

compileCode (Imp.SetMem dest src _) = do
  let src' = Var (compileName src)
  let dest' = Var (compileName dest)
  stm $ Reassign dest' src'

compileCode (Imp.Allocate name (Imp.Count e) DefaultSpace) = do
  e' <- compileExp e
  let allocate' = simpleCall "allocateMem" [e']
  let name' = Var (compileName name)
  stm $ Reassign name' allocate'

compileCode (Imp.Allocate name (Imp.Count e) (Imp.Space space)) =
  join $ asks envAllocate
    <*> pure name
    <*> compileExp e
    <*> pure space

compileCode (Imp.Copy dest (Imp.Count destoffset) DefaultSpace src (Imp.Count srcoffset) DefaultSpace (Imp.Count size)) = do
  destoffset' <- compileExp destoffset
  srcoffset' <- compileExp srcoffset
  let dest' = Var (compileName dest)
  let src' = Var (compileName src)
  size' <- compileExp size
  stm $ Exp $ simpleCall "Buffer.BlockCopy" [src', srcoffset', dest', destoffset', size']

compileCode (Imp.Copy dest (Imp.Count destoffset) destspace src (Imp.Count srcoffset) srcspace (Imp.Count size)) = do
  copy <- asks envCopy
  join $ copy
    <$> pure dest <*> compileExp destoffset <*> pure destspace
    <*> pure src <*> compileExp srcoffset <*> pure srcspace
    <*> compileExp size <*> pure (IntType Int64) -- FIXME

compileCode (Imp.Write dest (Imp.Count idx) elemtype DefaultSpace _ elemexp) = do
  idx' <- compileExp idx
  elemexp' <- compileExp elemexp
  let dest' = Var $ compileName dest
  let elemtype' = compileTypeConverter elemtype
  let ctype = simpleCall elemtype' [elemexp']
  stm $ Exp $ simpleCall "writeScalarArray" [dest', idx', ctype]

compileCode (Imp.Write dest (Imp.Count idx) elemtype (Imp.Space space) _ elemexp) =
  join $ asks envWriteScalar
    <*> pure dest
    <*> compileExp idx
    <*> pure elemtype
    <*> pure space
    <*> compileExp elemexp

compileCode Imp.Skip = return ()

-- | Public names must have a consitent prefix.
publicName :: String -> String
publicName s = "futhark_" ++ s

declMem :: VName -> Space -> CompilerM op s ()
declMem name DefaultSpace =
  stm $ AssignTyped (Composite $ ArrayT $ Primitive ByteT) (Var $ compileName name) Nothing
declMem name (Space _) =
  stm $ AssignTyped (CustomT "CLMemoryHandle") (Var $ compileName name) (Just $ Var "ctx.EMPTY_MEM_HANDLE")

memToCSType :: Space -> CompilerM op s CSType
memToCSType = return . rawMemCSType

rawMemCSType :: Space -> CSType
rawMemCSType DefaultSpace = Composite $ ArrayT $ Primitive ByteT
rawMemCSType (Space _) = CustomT "CLMemoryHandle"

toIntPtr :: CSExp -> CSExp
toIntPtr e = simpleInitClass "IntPtr" [e]
