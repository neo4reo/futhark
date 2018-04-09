{-# LANGUAGE FlexibleContexts #-}
module Futhark.CodeGen.Backends.CSOpenCL
  ( compileProg
  ) where

import Control.Monad
import Data.List


import Futhark.Error
import Futhark.Representation.ExplicitMemory (Prog, ExplicitMemory)
import Futhark.CodeGen.Backends.CSOpenCL.Boilerplate
import qualified Futhark.CodeGen.Backends.GenericCSharp as CS
import qualified Futhark.CodeGen.ImpCode.OpenCL as Imp
import qualified Futhark.CodeGen.ImpGen.OpenCL as ImpGen
import Futhark.CodeGen.Backends.GenericCSharp.AST
import Futhark.CodeGen.Backends.GenericCSharp.Options
import Futhark.CodeGen.Backends.GenericCSharp.Definitions
import Futhark.Util.Pretty(pretty)
import Futhark.MonadFreshNames hiding (newVName')


compileProg :: MonadFreshNames m => Maybe String
            -> Prog ExplicitMemory -> m (Either InternalError String)
compileProg module_name prog = do
  res <- ImpGen.compileProg prog
  case res of
    Left err -> return $ Left err
    Right (Imp.Program opencl_code opencl_prelude kernel_names types sizes prog') ->
      Right <$> CS.compileProg
                  module_name
                  CS.emptyConstructor
                  imports
                  defines
                  operations
                  ()
                  (generateBoilerplate opencl_code opencl_prelude kernel_names types sizes)
                  []
                  [Imp.Space "device", Imp.Space "local", Imp.DefaultSpace]
                  cliOptions
                  prog'

  where operations :: CS.Operations Imp.OpenCL ()
        operations = CS.defaultOperations
                     { CS.opsCompiler = callKernel
                     , CS.opsWriteScalar = writeOpenCLScalar
                     , CS.opsReadScalar = readOpenCLScalar
                     , CS.opsAllocate = allocateOpenCLBuffer
                     , CS.opsCopy = copyOpenCLMemory
                     , CS.opsStaticArray = staticOpenCLArray
                     , CS.opsEntryInput = unpackArrayInput
                     , CS.opsEntryOutput = packArrayOutput
                     }
        imports = [ Using Nothing "Cloo"
                  , Using Nothing "Cloo.Bindings"
                  , Using Nothing "System.Runtime.CompilerServices"
                  , Using Nothing "System.Runtime.InteropServices" ]
        defines = [ Escape csOpenCL
                  , Escape csPanic]

cliOptions :: [Option]
cliOptions = [ Option { optionLongName = "platform"
                      , optionShortName = Just 'p'
                      , optionArgument = RequiredArgument
                      , optionAction = [Escape "futhark_context_config_set_platform(ref cfg, optarg);"]
                      }
             , Option { optionLongName = "device"
                      , optionShortName = Just 'd'
                      , optionArgument = RequiredArgument
                      , optionAction = [Escape "futhark_context_config_set_device(ref cfg, optarg);"]
                      }
             , Option { optionLongName = "dump-opencl"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument
                      , optionAction = [Escape "futhark_context_config_dump_program_to(ref cfg, optarg);"]
                      }
             , Option { optionLongName = "load-opencl"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument
                      , optionAction = [Escape "futhark_context_config_load_program_from(ref cfg, optarg);"]
                      }
             , Option { optionLongName = "debugging"
                      , optionShortName = Just 'D'
                      , optionArgument = NoArgument
                      , optionAction = [Escape "futhark_context_config_set_debugging(ref cfg, true);"]
                      }
             ]

{-
             , Option { optionLongName = "default-group-size"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument
                      , optionAction = [C.cstm|futhark_context_config_set_default_group_size(cfg, atoi(optarg));|]
                      }
             , Option { optionLongName = "default-num-groups"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument
                      , optionAction = [C.cstm|futhark_context_config_set_default_num_groups(cfg, atoi(optarg));|]
                      }
             , Option { optionLongName = "default-tile-size"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument
                      , optionAction = [C.cstm|futhark_context_config_set_default_tile_size(cfg, atoi(optarg));|]
                      }
             , Option { optionLongName = "default-threshold"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument
                      , optionAction = [C.cstm|futhark_context_config_set_default_threshold(cfg, atoi(optarg));|]
                      }
             , Option { optionLongName = "print-sizes"
                      , optionShortName = Nothing
                      , optionArgument = NoArgument
                      , optionAction = [C.cstm|{
                          int n = futhark_get_num_sizes();
                          for (int i = 0; i < n; i++) {
                            printf("%s (%s)\n", futhark_get_size_name(i),
                                                futhark_get_size_class(i));
                          }
                          exit(0);
                        }|]
                      }
             , Option { optionLongName = "size"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument
                      , optionAction = [C.cstm|{
                          char *name = optarg;
                          char *equals = strstr(optarg, "=");
                          char *value_str = equals != NULL ? equals+1 : optarg;
                          int value = atoi(value_str);
                          if (equals != NULL) {
                            *equals = 0;
                            if (futhark_context_config_set_size(cfg, name, value) != 0) {
                              panic(1, "Unknown size: %s\n", name);
                            }
                          } else {
                            panic(1, "Invalid argument for size option: %s\n", optarg);
                          }}|]
                      }
             ]
-}

comment s = Comment s []

callKernel :: CS.OpCompiler Imp.OpenCL ()
callKernel (Imp.GetSize v key) = do
  CS.stm $ comment "callkernel getsize v key"
  CS.stm $ Reassign (Var (CS.compileName v)) $
    Field (Var "ctx.sizes") $ pretty key

callKernel (Imp.GetSizeMax v size_class) = do
  CS.stm $ comment "callkernel getsizemax v size_class"
  CS.stm $ Reassign (Var (CS.compileName v)) $
    Var $ "max_" ++ pretty size_class

callKernel (Imp.HostCode c) = do
  CS.stm $ comment "callkernel hostcode getsizemax v size_class"
  CS.compileCode c

callKernel (Imp.LaunchKernel name args kernel_size workgroup_size) = do
  CS.stm $ comment "callkernel"
  kernel_size' <- mapM CS.compileExp kernel_size
  let total_elements = foldl mult_exp (Integer 1) kernel_size'
  let cond = BinOp "!=" total_elements (Integer 0)
  workgroup_size' <- mapM CS.compileExp workgroup_size
  body <- CS.collect $ launchKernel name kernel_size' workgroup_size' args
  CS.stm $ If cond body []
  where mult_exp = BinOp "*"

launchKernel :: String -> [CSExp] -> [CSExp] -> [Imp.KernelArg] -> CS.CompilerM op s ()
launchKernel kernel_name kernel_dims workgroup_dims args = do
  CS.stm $ comment "launchkernel"
  let kernel_dims' = CreateArray (CS.compilePrimTypeToAST $ Imp.IntType Imp.Int32) kernel_dims
  let kernel_name' = "ctx."++kernel_name
  args_stms <- zipWithM (processKernelArg kernel_name') [0..] args

  CS.stm $ Unsafe $ concat args_stms

  global_work_size <- newVName' "global_work_size"
  local_work_size <- newVName' "local_work_size"
  stop_watch <- newVName' "stop_watch"
  time_diff <- newVName' "time_diff"

  let debugStartStmts =
        map Exp $ [CS.consoleErrorWrite "Launching {0} with global work size [" [String kernel_name]] ++
                  printKernelSize global_work_size ++
                  [ CS.consoleErrorWrite "] and local work size [" []] ++
                  printKernelSize local_work_size ++
                  [ CS.consoleErrorWrite "].\n" []
                  , CallMethod (Var stop_watch) (Var "Start") []]

  let ctx = (++) "ctx."
  let debugEndStmts =
          [ Exp $ CS.simpleCall "OPENCL_SUCCEED" [
              CS.simpleCall "CL10.Finish"
                [Var "ctx.opencl.queue"]]
          , Exp $ CallMethod (Var stop_watch) (Var "Stop") []
          , Assign (Var time_diff) $ asMicroseconds (Var stop_watch)
          , AssignOp "+" (Var $ ctx $ kernelRuntime kernel_name) (Var time_diff)
          , AssignOp "+" (Var $ ctx $ kernelRuns kernel_name) (Integer 1)
          , Exp $ CS.consoleErrorWriteLine "kernel {0} runtime: {1}" [String kernel_name, Var time_diff]
          ]


  CS.stm $ If (BinOp "!=" total_elements (Integer 0))
    ([ Assign (Var global_work_size) (Collection "IntPtr[]" $ map CS.toIntPtr kernel_dims)
     , Assign (Var local_work_size) (Collection "IntPtr[]" $ map CS.toIntPtr workgroup_dims)
     , Assign (Var stop_watch) $ CS.simpleInitClass "Stopwatch" []
     , If (Var "ctx.debugging") debugStartStmts []
     ]
     ++
     [ Exp $ CS.simpleCall "OPENCL_SUCCEED" [
         CS.simpleCall "CL10.EnqueueNDRangeKernel"
           [ Var "ctx.opencl.queue", Var kernel_name', Integer kernel_rank, Null
           , Var global_work_size, Var local_work_size, Integer 0, Null, Null]]]
     ++
     [ If (Var "ctx.debugging") debugEndStmts [] ]) []
  finishIfSynchronous

  where processKernelArg :: String
                         -> Integer
                         -> Imp.KernelArg
                         -> CS.CompilerM op s [CSStmt]
        processKernelArg kernel argnum (Imp.ValueKArg e bt) = do
          let t = CS.compilePrimTypeToAST bt
          tmp <- newVName' "kernel_arg"
          e' <- CS.compileExp e
          return [ AssignTyped t (Var tmp) (Just e')
                 , Comment "ValueKArg" []
                 , Exp $ getKernelCall kernel argnum (CS.sizeOf t) (Var tmp)]

        processKernelArg kernel argnum (Imp.MemKArg v) =
          return [ Comment "MemKArg" []
                 , Exp $ getKernelCall kernel argnum (CS.sizeOf $ Primitive IntPtrT) (Var $ CS.compileName v) ]

        processKernelArg kernel argnum (Imp.SharedMemoryKArg (Imp.Count num_bytes)) = do
          num_bytes' <- CS.compileExp num_bytes
          return [ Comment "SharedMemoryKArg" []
                 , Exp $ getKernelCall kernel argnum num_bytes' Null ]

        kernel_rank = toInteger $ length kernel_dims
        total_elements = foldl (BinOp "*") (Integer 1) kernel_dims

        printKernelSize :: String -> [CSExp]
        printKernelSize work_size =
          intersperse (CS.consoleErrorWrite ", " []) $ map (printKernelDim work_size) [0..kernel_rank-1]

        printKernelDim global_work_size i =
          CS.consoleErrorWrite "{0}" [Index (Var global_work_size) (IdxExp (Integer $ toInteger i))]

        asMicroseconds watch =
          BinOp "/" (Field watch "ElapsedTicks")
          (BinOp "/" (Field (Var "TimeSpan") "TicksPerMillisecond") (Integer 1000))



getKernelCall :: String -> Integer -> CSExp -> CSExp -> CSExp
getKernelCall kernel arg_num size Null =
  CS.simpleCall "CL10.SetKernelArg" [ Var kernel, Integer arg_num, CS.toIntPtr size, Var "ctx.NULL"]
getKernelCall kernel arg_num size e =
  CS.simpleCall "CL10.SetKernelArg" [ Var kernel, Integer arg_num, CS.toIntPtr size,CS.toIntPtr $ Addr e]

castToIntPtr :: CSExp -> CSExp
castToIntPtr = Cast (CustomT "IntPtr")

writeOpenCLScalar :: CS.WriteScalar Imp.OpenCL ()
writeOpenCLScalar mem i bt "device" val = do
  CS.stm $ comment "writescalar"
  let mem' = CS.compileName mem
  let bt' = CS.compilePrimTypeToAST bt
  scalar <- newVName' "scalar"
  ptr <- newVName' "ptr"
  CS.stm $ Unsafe
    [ AssignTyped bt' (Var scalar) (Just val)
    , AssignTyped (PointerT VoidT) (Var ptr) (Just $ Addr $ Var scalar)
    , Exp $ CS.simpleCall "CL10.EnqueueWriteBuffer"
        [ Var "ctx.opencl.queue", Var mem', Var "synchronous"
        ,CS.toIntPtr i,CS.toIntPtr $ CS.sizeOf bt',CS.toIntPtr $ Var ptr
    , Integer 0, Null, Null]
    ]

writeOpenCLScalar _ _ _ space _ =
  fail $ "Cannot write to '" ++ space ++ "' memory space."

readOpenCLScalar :: CS.ReadScalar Imp.OpenCL ()
readOpenCLScalar mem i bt "device" = do
  val <- newVName' "read_res"
  let bt' = CS.compilePrimTypeToAST bt
  let mem' = CS.compileName mem
  CS.stm $ comment "readscalar"
  CS.stm $ AssignTyped bt' (Var val) (Just $ CS.simpleInitClass (pretty bt') [])
  CS.stm $ Exp $ CS.simpleCall "CL10.EnqueueReadBuffer"
             [ Var "ctx.opencl.queue", Var mem' , Bool True
             ,CS.toIntPtr i, CS.sizeOf bt',CS.toIntPtr $ Ref $ Var val
             , Integer 0, Null, Null]
  return $ Var val

readOpenCLScalar _ _ _ space =
  fail $ "Cannot read from '" ++ space ++ "' memory space."

computeErrCodeT = CustomT "ComputeErrorCode"

allocateOpenCLBuffer :: CS.Allocate Imp.OpenCL ()
allocateOpenCLBuffer mem size "device" = do
  CS.stm $ comment "allocatebuffer"
  errcode <- CS.compileName <$> newVName "err_code"
  CS.stm $ AssignTyped computeErrCodeT (Var errcode) Nothing
  CS.stm $ Reassign (Var $ CS.compileName mem) $
    CS.simpleCall "CL10.CreateBuffer" [ Var "ctx.opencl.context", Var "ComputeMemoryFlags.ReadWrite"
                                      ,CS.toIntPtr size, Var "IntPtr.Zero"
                                      , Out $ Var errcode ]

allocateOpenCLBuffer _ _ space =
  fail $ "Cannot allocate in '" ++ space ++ "' space"

copyOpenCLMemory :: CS.Copy Imp.OpenCL ()
copyOpenCLMemory destmem destidx Imp.DefaultSpace srcmem srcidx (Imp.Space "device") nbytes bt = do
  let srcmem'  = Var $ CS.compileName srcmem
  let destmem' = Var $ CS.compileName destmem
  ptr <- newVName' "ptr"
  CS.stm $ comment "copymemory def to device"
  CS.stm $ Fixed (CS.assignArrayPointer destmem' (Var ptr))
    [ ifNotZeroSize nbytes $
      Exp $ CS.simpleCall "CL10.EnqueueReadBuffer"
      [ Var "ctx.opencl.queue", srcmem', Var "synchronous"
      ,CS.toIntPtr srcidx, nbytes,CS.toIntPtr $ Var ptr
      ,CS.toIntPtr destidx, Null, Null]
    ]

copyOpenCLMemory destmem destidx (Imp.Space "device") srcmem srcidx Imp.DefaultSpace nbytes _ = do
  let destmem' = CS.compileName destmem
  let srcmem'  = CS.compileName srcmem
  ptr <- newVName' "ptr"
  CS.stm $ comment "copymemory device to def"
  CS.stm $ Fixed (CS.assignArrayPointer (Var srcmem') (Var ptr))
    [ ifNotZeroSize nbytes $
      Exp $ CS.simpleCall "CL10.EnqueueWriteBuffer"
        [ Var "queue", Var destmem', Var "synchronous"
        ,CS.toIntPtr destidx, nbytes,CS.toIntPtr $ Var ptr
        , srcidx, Null, Null]
    ]

copyOpenCLMemory destmem destidx (Imp.Space "device") srcmem srcidx (Imp.Space "device") nbytes _ = do
  let destmem' = CS.compileName destmem
  let srcmem'  = CS.compileName srcmem

  CS.stm $ comment "copymemory device to device"
  CS.stm $ ifNotZeroSize nbytes $
    Exp $ CS.simpleCall "CL10.EnqueueCopyBuffer"
      [ Var "ctx.opencl.queue", Var srcmem', Var destmem'
      ,CS.toIntPtr srcidx,CS.toIntPtr destidx,CS.toIntPtr nbytes
      , Integer 0, Null, Null]
  finishIfSynchronous

copyOpenCLMemory destmem destidx Imp.DefaultSpace srcmem srcidx Imp.DefaultSpace nbytes _ =
  CS.copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes

copyOpenCLMemory _ _ destspace _ _ srcspace _ _=
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace

staticOpenCLArray :: CS.StaticArray Imp.OpenCL ()
staticOpenCLArray name "device" t vs = do
  mapM_ CS.atInit <=< CS.collect $ do
    -- Create host-side C# array with intended values.
    tmp_arr <- newVName' "tmp_arr"
    CS.stm $ Assign (Var tmp_arr) $
      CreateArray (CS.compilePrimTypeToAST t) $ map CS.compilePrimValue vs

    -- Create memory block on the device.
    static_mem <- newVName "static_mem"
    ptr <- newVName' "ptr"
    let size = Integer $ genericLength vs * Imp.primByteSize t
    allocateOpenCLBuffer static_mem size "device"

    -- Copy Numpy array to the device memory block.
    CS.stm $ comment "staticarray"
    CS.stm $ Fixed (CS.assignArrayPointer (Var tmp_arr) (Var ptr))
      [ ifNotZeroSize size $
          Exp $ CS.simpleCall "CL10.EnqueueWriteBuffer"
          [ Var "ctx.opencl.queue", Var $ CS.compileName static_mem, Var "synchronous"
          ,CS.toIntPtr (Integer 0),CS.toIntPtr size
          , Var ptr, Integer 0, Null, Null ]

                                                  ]
    -- Store the memory block for later reference.
    CS.stm $ Assign (Var name') $ Var $ CS.compileName static_mem

  CS.stm $ Reassign (Var name') (Var name')
  where name' = CS.compileName name
staticOpenCLArray _ space _ _ =
  fail $ "CS.penCL backend cannot create static array in memory space '" ++ space ++ "'"

packArrayOutput :: CS.EntryOutput Imp.OpenCL ()
packArrayOutput mem "device" bt ept dims = do
  let size = foldr (BinOp "*") (Integer 1) dims'
  let bt' = CS.compilePrimTypeToASText bt ept
  let nbytes = BinOp "*" (CS.sizeOf bt') size
  let createArray = "createArray_"++ pretty bt'

  return $ CS.simpleCall createArray [ Var $ CS.compileName mem, Var "ctx.opencl.queue"
                                     , Var "synchronous", nbytes
                                     , CreateArray (Primitive $ CSInt Int64T) dims']
  where dims' = map CS.compileDim dims

packArrayOutput _ sid _ _ _ =
  fail $ "Cannot return array from " ++ sid ++ " space."

unpackArrayInput :: CS.EntryInput Imp.OpenCL ()
unpackArrayInput mem memsize "device" t _ dims e = do
  let size = foldr (BinOp "*") (Integer 1) dims'
  let t' = CS.compilePrimTypeToAST t
  let nbytes = BinOp "*" (CS.sizeOf t') size
  zipWithM_ (CS.unpackDim e) dims [0..]
  ptr <- pretty <$> newVName "ptr"

  let memsize' = CS.compileDim memsize
  allocateOpenCLBuffer mem memsize' "device"
  CS.stm $ comment "packarrinput"
  CS.stm $ Unsafe [Fixed (CS.assignArrayPointer (Field e "array") (Var ptr))
      [ ifNotZeroSize memsize' $
        Exp $ CS.simpleCall "CL10.EnqueueWriteBuffer"
        [ Var "ctx.opencl.queue", Var $ CS.compileName mem, Var "synchronous"
        ,CS.toIntPtr (Integer 0), memsize',CS.toIntPtr (Var ptr)
        ,CS.toIntPtr (Integer 0), Null, Null]
      ]]
  CS.stm $ Comment "suppe" []

  where mem_dest = Var $ CS.compileName mem
        dims' = map CS.compileDim dims
unpackArrayInput _ _ sid _ _ _ _ =
  fail $ "Cannot accept array from " ++ sid ++ " space."

ifNotZeroSize :: CSExp -> CSStmt -> CSStmt
ifNotZeroSize e s =
  If (BinOp "!=" e (Integer 0)) [s] []

finishIfSynchronous :: CS.CompilerM op s ()
finishIfSynchronous =
  CS.stm $ If (Var "synchronous") [Exp $ CS.simpleCall "CL10.Finish" [Var "ctx.opencl.queue"]] []

newVName' s = CS.compileName <$> newVName s
