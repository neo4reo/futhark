{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Futhark.Construct
  ( letSubExp
  , letSubExps
  , letExp
  , letExps
  , letTupExp
  , letTupExp'
  , letInPlace

  , eSubExp
  , eIf
  , eIf'
  , eBinOp
  , eCmpOp
  , eNegate
  , eNot
  , eAbs
  , eSignum
  , eCopy
  , eAssert
  , eValue
  , eBody
  , eLambda
  , eDivRoundingUp
  , eRoundToMultipleOf
  , eSliceArray
  , eSplitArray

  , eWriteArray

  , asIntZ, asIntS

  , resultBody
  , resultBodyM
  , insertStmsM
  , mapResult

  , foldBinOp
  , binOpLambda
  , cmpOpLambda
  , fullSlice
  , fullSliceNum
  , isFullSlice
  , ifCommon
  , nilFn

  , module Futhark.Binder

  -- * Result types
  , instantiateShapes
  , instantiateShapes'
  , instantiateShapesFromIdentList
  , instantiateExtTypes
  , instantiateIdents
  , removeExistentials

  -- * Convenience
  , simpleMkLetNames

  , ToExp(..)
  )
where

import qualified Data.Array as A
import qualified Data.Map.Strict as M
import Data.Loc (SrcLoc)
import Data.List
import Data.Ord
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

import Futhark.Representation.AST
import Futhark.MonadFreshNames
import Futhark.Binder
import Futhark.Util

letSubExp :: MonadBinder m =>
             String -> Exp (Lore m) -> m SubExp
letSubExp _ (BasicOp (SubExp se)) = return se
letSubExp desc e = Var <$> letExp desc e

letExp :: MonadBinder m =>
          String -> Exp (Lore m) -> m VName
letExp _ (BasicOp (SubExp (Var v))) =
  return v
letExp desc e = do
  n <- length <$> expExtType e
  vs <- replicateM n $ newVName desc
  idents <- letBindNames vs e
  case idents of
    [ident] -> return $ identName ident
    _       -> fail $ "letExp: tuple-typed expression given:\n" ++ pretty e

letInPlace :: MonadBinder m =>
              String -> VName -> Slice SubExp -> Exp (Lore m)
           -> m VName
letInPlace desc src slice e = do
  tmp <- letSubExp (desc ++ "_tmp") e
  letExp desc $ BasicOp $ Update src slice tmp

letSubExps :: MonadBinder m =>
              String -> [Exp (Lore m)] -> m [SubExp]
letSubExps desc = mapM $ letSubExp desc

letExps :: MonadBinder m =>
           String -> [Exp (Lore m)] -> m [VName]
letExps desc = mapM $ letExp desc

letTupExp :: (MonadBinder m) =>
             String -> Exp (Lore m)
          -> m [VName]
letTupExp _ (BasicOp (SubExp (Var v))) =
  return [v]
letTupExp name e = do
  numValues <- length <$> expExtType e
  names <- replicateM numValues $ newVName name
  map identName <$> letBindNames names e

letTupExp' :: (MonadBinder m) =>
              String -> Exp (Lore m)
           -> m [SubExp]
letTupExp' _ (BasicOp (SubExp se)) = return [se]
letTupExp' name ses = map Var <$> letTupExp name ses

eSubExp :: MonadBinder m =>
           SubExp -> m (Exp (Lore m))
eSubExp = pure . BasicOp . SubExp

eIf :: (MonadBinder m, BranchType (Lore m) ~ ExtType) =>
       m (Exp (Lore m)) -> m (Body (Lore m)) -> m (Body (Lore m))
    -> m (Exp (Lore m))
eIf ce te fe = eIf' ce te fe IfNormal

-- | As 'eIf', but an 'IfSort' can be given.
eIf' :: (MonadBinder m, BranchType (Lore m) ~ ExtType) =>
        m (Exp (Lore m)) -> m (Body (Lore m)) -> m (Body (Lore m))
     -> IfSort
     -> m (Exp (Lore m))
eIf' ce te fe if_sort = do
  ce' <- letSubExp "cond" =<< ce
  te' <- insertStmsM te
  fe' <- insertStmsM fe
  -- We need to construct the context.
  ts <- generaliseExtTypes <$> bodyExtType te' <*> bodyExtType fe'
  te'' <- addContextForBranch ts te'
  fe'' <- addContextForBranch ts fe'
  return $ If ce' te'' fe'' $ IfAttr ts if_sort
  where addContextForBranch ts (Body _ stms val_res) = do
          body_ts <- extendedScope (traverse subExpType val_res) stmsscope
          let ctx_res = map snd $ sortBy (comparing fst) $
                        M.toList $ shapeExtMapping ts body_ts
          mkBodyM stms $ ctx_res++val_res
            where stmsscope = scopeOf stms

eBinOp :: MonadBinder m =>
          BinOp -> m (Exp (Lore m)) -> m (Exp (Lore m))
       -> m (Exp (Lore m))
eBinOp op x y = do
  x' <- letSubExp "x" =<< x
  y' <- letSubExp "y" =<< y
  return $ BasicOp $ BinOp op x' y'

eCmpOp :: MonadBinder m =>
          CmpOp -> m (Exp (Lore m)) -> m (Exp (Lore m))
       -> m (Exp (Lore m))
eCmpOp op x y = do
  x' <- letSubExp "x" =<< x
  y' <- letSubExp "y" =<< y
  return $ BasicOp $ CmpOp op x' y'

eNegate :: MonadBinder m =>
           m (Exp (Lore m)) -> m (Exp (Lore m))
eNegate em = do
  e <- em
  e' <- letSubExp "negate_arg" e
  t <- subExpType e'
  case t of
    Prim (IntType int_t) ->
      return $ BasicOp $
      BinOp (Sub int_t) (intConst int_t 0) e'
    Prim (FloatType float_t) ->
      return $ BasicOp $
      BinOp (FSub float_t) (floatConst float_t 0) e'
    _ ->
      fail $ "eNegate: operand " ++ pretty e ++ " has invalid type."

eNot :: MonadBinder m =>
        m (Exp (Lore m)) -> m (Exp (Lore m))
eNot e = BasicOp . UnOp Not <$> (letSubExp "not_arg" =<< e)

eAbs :: MonadBinder m =>
        m (Exp (Lore m)) -> m (Exp (Lore m))
eAbs em = do
  e <- em
  e' <- letSubExp "abs_arg" e
  t <- subExpType e'
  case t of
    Prim (IntType int_t) ->
      return $ BasicOp $ UnOp (Abs int_t) e'
    Prim (FloatType float_t) ->
      return $ BasicOp $ UnOp (FAbs float_t) e'
    _ ->
      fail $ "eAbs: operand " ++ pretty e ++ " has invalid type."

eSignum :: MonadBinder m =>
        m (Exp (Lore m)) -> m (Exp (Lore m))
eSignum em = do
  e <- em
  e' <- letSubExp "signum_arg" e
  t <- subExpType e'
  case t of
    Prim (IntType int_t) ->
      return $ BasicOp $ UnOp (SSignum int_t) e'
    _ ->
      fail $ "eSignum: operand " ++ pretty e ++ " has invalid type."

eCopy :: MonadBinder m =>
         m (Exp (Lore m)) -> m (Exp (Lore m))
eCopy e = BasicOp . Copy <$> (letExp "copy_arg" =<< e)

eAssert :: MonadBinder m =>
         m (Exp (Lore m)) -> String -> SrcLoc -> m (Exp (Lore m))
eAssert e msg loc = do e' <- letSubExp "assert_arg" =<< e
                       return $ BasicOp $ Assert e' msg (loc, mempty)

eValue :: MonadBinder m => Value -> m (Exp (Lore m))
eValue (PrimVal bv) =
  return $ BasicOp $ SubExp $ Constant bv
eValue (ArrayVal a bt [_]) = do
  let ses = map Constant $ A.elems a
  return $ BasicOp $ ArrayLit ses $ Prim bt
eValue (ArrayVal a bt shape) = do
  let rowshape = drop 1 shape
      rowsize  = product rowshape
      rows     = [ ArrayVal (A.listArray (0,rowsize-1) r) bt rowshape
                 | r <- chunk rowsize $ A.elems a ]
      rowtype = Array bt (Shape $ map (intConst Int32 . toInteger) rowshape)
                NoUniqueness
  ses <- mapM (letSubExp "array_elem" <=< eValue) rows
  return $ BasicOp $ ArrayLit ses rowtype

eBody :: (MonadBinder m) =>
         [m (Exp (Lore m))]
      -> m (Body (Lore m))
eBody es = insertStmsM $ do
             es' <- sequence es
             xs <- mapM (letTupExp "x") es'
             mkBodyM mempty $ map Var $ concat xs

eLambda :: MonadBinder m =>
           Lambda (Lore m) -> [m (Exp (Lore m))] -> m [SubExp]
eLambda lam args = do zipWithM_ bindParam (lambdaParams lam) args
                      bodyBind $ lambdaBody lam
  where bindParam param arg = letBindNames_ [paramName param] =<< arg

-- | Note: unsigned division.
eDivRoundingUp :: MonadBinder m =>
                  IntType -> m (Exp (Lore m)) -> m (Exp (Lore m)) -> m (Exp (Lore m))
eDivRoundingUp t x y =
  eBinOp (SQuot t) (eBinOp (Add t) x (eBinOp (Sub t) y (eSubExp one))) y
  where one = intConst t 1

eRoundToMultipleOf :: MonadBinder m =>
                      IntType -> m (Exp (Lore m)) -> m (Exp (Lore m)) -> m (Exp (Lore m))
eRoundToMultipleOf t x d =
  ePlus x (eMod (eMinus d (eMod x d)) d)
  where eMod = eBinOp (SMod t)
        eMinus = eBinOp (Sub t)
        ePlus = eBinOp (Add t)

-- | Construct an 'Index' expressions that slices an array with unit stride.
eSliceArray :: MonadBinder m =>
               Int -> VName -> m (Exp (Lore m)) -> m (Exp (Lore m))
            -> m (Exp (Lore m))
eSliceArray d arr i n = do
  arr_t <- lookupType arr
  let skips = map (slice (constant (0::Int32))) $ take d $ arrayDims arr_t
  i' <- letSubExp "slice_i" =<< i
  n' <- letSubExp "slice_n" =<< n
  return $ BasicOp $ Index arr $ fullSlice arr_t $ skips ++ [slice i' n']
  where slice j m = DimSlice j m (constant (1::Int32))

-- | Construct an 'Index' expressions that splits an array in different parts along the outer dimension.
eSplitArray :: MonadBinder m =>
               VName -> [m (Exp (Lore m))] -> m [Exp (Lore m)]
eSplitArray arr sizes = do
  sizes' <- mapM (letSubExp "split_size") =<< sequence sizes
  -- Compute the starting offset for each slice.
  (_, offsets) <- mapAccumLM increase (intConst Int32 0) sizes'
  zipWithM (eSliceArray 0 arr) (map eSubExp offsets) (map eSubExp sizes')
  where increase offset size = do
          offset' <- letSubExp "offset" $ BasicOp $ BinOp (Add Int32) offset size
          return (offset', offset)

-- | Write to an index of the array, if within bounds.  Otherwise,
-- nothing.  Produces the updated array.
eWriteArray :: (MonadBinder m, BranchType (Lore m) ~ ExtType) =>
               VName -> [m (Exp (Lore m))] -> m (Exp (Lore m))
            -> m (Exp (Lore m))
eWriteArray arr is v = do
  arr_t <- lookupType arr
  let ws = arrayDims arr_t
  is' <- mapM (letSubExp "write_i") =<< sequence is
  v' <- letSubExp "write_v" =<< v
  let checkDim w i = do
        less_than_zero <- letSubExp "less_than_zero" $
          BasicOp $ CmpOp (CmpSlt Int32) i (constant (0::Int32))
        greater_than_size <- letSubExp "greater_than_size" $
          BasicOp $ CmpOp (CmpSle Int32) w i
        letSubExp "outside_bounds_dim" $
          BasicOp $ BinOp LogOr less_than_zero greater_than_size

  outside_bounds <-
    letSubExp "outside_bounds" =<<
    foldBinOp LogOr (constant False) =<<
    zipWithM checkDim ws is'

  outside_bounds_branch <- insertStmsM $ resultBodyM [Var arr]

  in_bounds_branch <- insertStmsM $ do
    res <- letInPlace "write_out_inside_bounds" arr
           (fullSlice arr_t (map DimFix is')) $ BasicOp $ SubExp v'
    resultBodyM [Var res]

  return $
    If outside_bounds outside_bounds_branch in_bounds_branch $
    ifCommon [arr_t]

-- | Sign-extend to the given integer type.
asIntS :: MonadBinder m => IntType -> SubExp -> m SubExp
asIntS = asInt SExt

-- | Zero-extend to the given integer type.
asIntZ :: MonadBinder m => IntType -> SubExp -> m SubExp
asIntZ = asInt ZExt

asInt :: MonadBinder m =>
         (IntType -> IntType -> ConvOp) -> IntType -> SubExp -> m SubExp
asInt ext to_it e = do
  e_t <- subExpType e
  case e_t of
    Prim (IntType from_it)
      | to_it == from_it -> return e
      | otherwise -> letSubExp s $ BasicOp $ ConvOp (ext from_it to_it) e
    _ -> fail "asInt: wrong type"
  where s = case e of Var v -> baseString v
                      _     -> "to_" ++ pretty to_it


-- | Apply a binary operator to several subexpressions.  A left-fold.
foldBinOp :: MonadBinder m =>
             BinOp -> SubExp -> [SubExp] -> m (Exp (Lore m))
foldBinOp _ ne [] =
  return $ BasicOp $ SubExp ne
foldBinOp bop ne (e:es) =
  eBinOp bop (pure $ BasicOp $ SubExp e) (foldBinOp bop ne es)

-- | Create a two-parameter lambda whose body applies the given binary
-- operation to its arguments.  It is assumed that both argument and
-- result types are the same.  (This assumption should be fixed at
-- some point.)
binOpLambda :: (MonadBinder m, Bindable (Lore m)) =>
               BinOp -> PrimType -> m (Lambda (Lore m))
binOpLambda bop t = binLambda (BinOp bop) t t

-- | As 'binOpLambda', but for 'CmpOp's.
cmpOpLambda :: (MonadBinder m, Bindable (Lore m)) =>
               CmpOp -> PrimType -> m (Lambda (Lore m))
cmpOpLambda cop t = binLambda (CmpOp cop) t Bool

binLambda :: (MonadBinder m, Bindable (Lore m)) =>
             (SubExp -> SubExp -> BasicOp (Lore m)) -> PrimType -> PrimType
          -> m (Lambda (Lore m))
binLambda bop arg_t ret_t = do
  x   <- newVName "x"
  y   <- newVName "y"
  body <- insertStmsM $ do
    res <- letSubExp "res" $ BasicOp $ bop (Var x) (Var y)
    return $ resultBody [res]
  return Lambda {
             lambdaParams     = [Param x (Prim arg_t),
                                 Param y (Prim arg_t)]
           , lambdaReturnType = [Prim ret_t]
           , lambdaBody       = body
           }

-- | @fullSlice t slice@ returns @slice@, but with 'DimSlice's of
-- entire dimensions appended to the full dimensionality of @t@.  This
-- function is used to turn incomplete indexing complete, as required
-- by 'Index'.
fullSlice :: Type -> [DimIndex SubExp] -> Slice SubExp
fullSlice t slice =
  slice ++
  map (\d -> DimSlice (constant (0::Int32)) d (constant (1::Int32)))
  (drop (length slice) $ arrayDims t)

-- | Like 'fullSlice', but the dimensions are simply numeric.
fullSliceNum :: Num d => [d] -> [DimIndex d] -> Slice d
fullSliceNum dims slice =
  slice ++ map (\d -> DimSlice 0 d 1) (drop (length slice) dims)

-- | Does the slice describe the full size of the array?  The most
-- obvious such slice is one that 'DimSlice's the full span of every
-- dimension, but also one that fixes all unit dimensions.
isFullSlice :: Shape -> Slice SubExp -> Bool
isFullSlice shape slice = and $ zipWith allOfIt (shapeDims shape) slice
  where allOfIt (Constant v) DimFix{} = oneIsh v
        allOfIt d (DimSlice _ n _) = d == n
        allOfIt _ _ = False

ifCommon :: [Type] -> IfAttr ExtType
ifCommon ts = IfAttr (staticShapes ts) IfNormal

-- | A lambda with no parameters that returns no values.
nilFn :: Bindable lore => LambdaT lore
nilFn = Lambda mempty (mkBody mempty mempty) mempty

-- | Conveniently construct a body that contains no bindings.
resultBody :: Bindable lore => [SubExp] -> Body lore
resultBody = mkBody mempty

-- | Conveniently construct a body that contains no bindings - but
-- this time, monadically!
resultBodyM :: MonadBinder m =>
               [SubExp]
            -> m (Body (Lore m))
resultBodyM = mkBodyM mempty

-- | Evaluate the action, producing a body, then wrap it in all the
-- bindings it created using 'addStm'.
insertStmsM :: (MonadBinder m) =>
               m (Body (Lore m)) -> m (Body (Lore m))
insertStmsM m = do
  (Body _ bnds res, otherbnds) <- collectStms m
  mkBodyM (otherbnds <> bnds) res

-- | Change that result where evaluation of the body would stop.  Also
-- change type annotations at branches.
mapResult :: Bindable lore =>
             (Result -> Body lore) -> Body lore -> Body lore
mapResult f (Body _ bnds res) =
  let Body _ bnds2 newres = f res
  in mkBody (bnds<>bnds2) newres

-- | Instantiate all existential parts dimensions of the given
-- type, using a monadic action to create the necessary 'SubExp's.
-- You should call this function within some monad that allows you to
-- collect the actions performed (say, 'Writer').
instantiateShapes :: Monad m =>
                     (Int -> m SubExp)
                  -> [TypeBase ExtShape u]
                  -> m [TypeBase Shape u]
instantiateShapes f ts = evalStateT (mapM instantiate ts) M.empty
  where instantiate t = do
          shape <- mapM instantiate' $ shapeDims $ arrayShape t
          return $ t `setArrayShape` Shape shape
        instantiate' (Ext x) = do
          m <- get
          case M.lookup x m of
            Just se -> return se
            Nothing -> do se <- lift $ f x
                          put $ M.insert x se m
                          return se
        instantiate' (Free se) = return se

instantiateShapes' :: MonadFreshNames m =>
                      [TypeBase ExtShape u]
                   -> m ([TypeBase Shape u], [Ident])
instantiateShapes' ts =
  runWriterT $ instantiateShapes instantiate ts
  where instantiate _ = do v <- lift $ newIdent "size" $ Prim int32
                           tell [v]
                           return $ Var $ identName v

instantiateShapesFromIdentList :: [Ident] -> [ExtType] -> [Type]
instantiateShapesFromIdentList idents ts =
  evalState (instantiateShapes instantiate ts) idents
  where instantiate _ = do
          idents' <- get
          case idents' of
            [] -> fail "instantiateShapesFromIdentList: insufficiently sized context"
            ident:idents'' -> do put idents''
                                 return $ Var $ identName ident

instantiateExtTypes :: [VName] -> [ExtType] -> [Ident]
instantiateExtTypes names rt =
  let (shapenames,valnames) = splitAt (shapeContextSize rt) names
      shapes = [ Ident name (Prim int32) | name <- shapenames ]
      valts  = instantiateShapesFromIdentList shapes rt
      vals   = [ Ident name t | (name,t) <- zip valnames valts ]
  in shapes ++ vals

instantiateIdents :: [VName] -> [ExtType]
                  -> Maybe ([Ident], [Ident])
instantiateIdents names ts
  | let n = shapeContextSize ts,
    n + length ts == length names = do
    let (context, vals) = splitAt n names
        nextShape _ = do
          (context', remaining) <- get
          case remaining of []   -> lift Nothing
                            x:xs -> do let ident = Ident x (Prim int32)
                                       put (context'++[ident], xs)
                                       return $ Var x
    (ts', (context', _)) <-
      runStateT (instantiateShapes nextShape ts) ([],context)
    return (context', zipWith Ident vals ts')
  | otherwise = Nothing

removeExistentials :: ExtType -> Type -> Type
removeExistentials t1 t2 =
  t1 `setArrayDims`
  zipWith nonExistential
  (shapeDims $ arrayShape t1)
  (arrayDims t2)
  where nonExistential (Ext _)    dim = dim
        nonExistential (Free dim) _   = dim

-- | Can be used as the definition of 'mkLetNames' for a 'Bindable'
-- instance for simple representations.
simpleMkLetNames :: (ExpAttr lore ~ (), LetAttr lore ~ Type,
                     MonadFreshNames m, TypedOp (Op lore), HasScope lore m) =>
                    [VName] -> Exp lore -> m (Stm lore)
simpleMkLetNames names e = do
  et <- expExtType e
  (ts, shapes) <- instantiateShapes' et
  let shapeElems = [ PatElem shape shapet | Ident shape shapet <- shapes ]
  let valElems = zipWith PatElem names ts
  return $ Let (Pattern shapeElems valElems) (StmAux mempty ()) e

-- | Instances of this class can be converted to Futhark expressions
-- within a 'MonadBinder'.
class ToExp a where
  toExp :: MonadBinder m => a -> m (Exp (Lore m))

instance ToExp SubExp where
  toExp = return . BasicOp . SubExp

instance ToExp VName where
  toExp = return . BasicOp . SubExp . Var
