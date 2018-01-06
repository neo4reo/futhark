{-# LANGUAGE FlexibleContexts #-}
module Futhark.Internalise.Lambdas
  ( InternaliseLambda
  , internaliseMapLambda
  , internaliseStreamMapLambda
  , internaliseFoldLambda
  , internaliseStreamLambda
  , internalisePartitionLambdas
  )
  where

import Control.Monad
import Data.Monoid
import Data.Loc
import qualified Data.Set as S

import Language.Futhark as E
import Futhark.Representation.SOACS as I
import Futhark.MonadFreshNames

import Futhark.Internalise.Monad
import Futhark.Internalise.AccurateSizes
import Futhark.Representation.SOACS.Simplify (simplifyLambda)

-- | A function for internalising lambdas.
type InternaliseLambda =
  E.Exp -> [I.Type] -> InternaliseM ([I.LParam], I.Body, [I.ExtType])

internaliseMapLambda :: InternaliseLambda
                     -> E.Exp
                     -> [I.SubExp]
                     -> InternaliseM I.Lambda
internaliseMapLambda =
  internaliseSomeMapLambda index0 id I.rowType
  where index0 arg = do
          arg' <- letExp "arg" $ I.BasicOp $ I.SubExp arg
          arg_t <- lookupType arg'
          letSubExp "elem" $ I.BasicOp $ I.Index arg' $ fullSlice arg_t [I.DimFix zero]
        zero = constant (0::I.Int32)

internaliseStreamMapLambda :: InternaliseLambda
                           -> E.Exp
                           -> [I.SubExp]
                           -> InternaliseM I.Lambda
internaliseStreamMapLambda internaliseLambda lam args = do
  chunk_size <- newVName "chunk_size"
  lam' <- internaliseSomeMapLambda slice0 (`setOuterSize` I.Var chunk_size) id
          internaliseLambda lam args
  return lam' { lambdaParams = I.Param chunk_size (I.Prim int32) : lambdaParams lam' }
  where slice0 arg = do
          arg' <- letExp "arg" $ I.BasicOp $ I.SubExp arg
          arg_t <- lookupType arg'
          let w = arraySize 0 arg_t
          letSubExp "elem" $ I.BasicOp $ I.Index arg' $
            fullSlice arg_t [I.DimSlice zero w one]
        zero = constant (0::I.Int32)
        one = constant (1::I.Int32)

internaliseSomeMapLambda :: (SubExp -> InternaliseM SubExp)
                         -> (Type -> Type)
                         -> (Type -> Type)
                         -> InternaliseLambda
                         -> E.Exp
                         -> [I.SubExp]
                         -> InternaliseM I.Lambda
internaliseSomeMapLambda indexArg extReturn rowType' internaliseLambda lam args = do
  argtypes <- mapM I.subExpType args
  let rowtypes = map rowType' argtypes
  (params, body, rettype) <- internaliseLambda lam rowtypes
  (rettype', inner_shapes) <- instantiateShapes' rettype
  let outer_shape = arraysSize 0 argtypes
  shape_body <- bindingParamTypes params $
                shapeBody (map I.identName inner_shapes) rettype' body
  shapefun <- makeShapeFun params shape_body (length inner_shapes)
  bindMapShapes indexArg inner_shapes shapefun args outer_shape
  body' <- bindingParamTypes params $
           ensureResultShape asserting "not all iterations produce same shape"
           (srclocOf lam) (map extReturn rettype') body
  return $ I.Lambda params body' rettype'

makeShapeFun :: [I.LParam] -> I.Body -> Int
             -> InternaliseM I.Lambda
makeShapeFun params body n = do
  -- Some of 'params' may be unique, which means that the shape slice
  -- would consume its input.  This is not acceptable - that input is
  -- needed for the value function!  Hence, for all unique parameters,
  -- we create a substitute non-unique parameter, and insert a
  -- copy-binding in the body of the function.
  (params', copybnds) <- nonuniqueParams params
  return $ I.Lambda params' (insertStms copybnds body) rettype
  where rettype = replicate n $ I.Prim int32

bindMapShapes :: (SubExp -> InternaliseM SubExp)
              -> [I.Ident] -> I.Lambda -> [I.SubExp] -> SubExp
              -> InternaliseM ()
bindMapShapes indexArg inner_shapes sizefun args outer_shape
  | null $ I.lambdaReturnType sizefun = return ()
  | otherwise = do
      let size_args = replicate (length $ lambdaParams sizefun) Nothing
      sizefun' <- simplifyLambda sizefun size_args
      let sizefun_safe =
            all (I.safeExp . I.stmExp) $ I.bodyStms $ I.lambdaBody sizefun'
          sizefun_arg_invariant =
            not $ any (`S.member` freeInBody (I.lambdaBody sizefun')) $
            map I.paramName $ lambdaParams sizefun'
      if sizefun_safe && sizefun_arg_invariant
        then do ses <- bodyBind $ lambdaBody sizefun'
                forM_ (zip inner_shapes ses) $ \(v, se) ->
                  letBind_ (basicPattern' [] [v]) $ I.BasicOp $ I.SubExp se
        else letBind_ (basicPattern' [] inner_shapes) =<<
             eIf' isnonempty nonemptybranch emptybranch IfFallback

  where emptybranch =
          pure $ resultBody (map (const zero) $ I.lambdaReturnType sizefun)
        nonemptybranch = insertStmsM $
          resultBody <$> (eLambda sizefun =<< mapM indexArg args)

        isnonempty = eNot $ eCmpOp (I.CmpEq I.int32)
                     (pure $ I.BasicOp $ I.SubExp outer_shape)
                     (pure $ I.BasicOp $ SubExp zero)
        zero = constant (0::I.Int32)

internaliseFoldLambda :: InternaliseLambda
                      -> E.Exp
                      -> [I.Type] -> [I.Type]
                      -> InternaliseM I.Lambda
internaliseFoldLambda internaliseLambda lam acctypes arrtypes = do
  let rowtypes = map I.rowType arrtypes
  (params, body, rettype) <- internaliseLambda lam $ acctypes ++ rowtypes
  let rettype' = [ t `I.setArrayShape` I.arrayShape shape
                   | (t,shape) <- zip rettype acctypes ]
  -- The result of the body must have the exact same shape as the
  -- initial accumulator.  We accomplish this with an assertion and
  -- reshape().
  body' <- bindingParamTypes params $
           ensureResultShape asserting
           "shape of result does not match shape of initial value" (srclocOf lam) rettype' body
  return $ I.Lambda params body' rettype'

internaliseStreamLambda :: InternaliseLambda
                        -> E.Exp
                        -> [I.Type]
                        -> InternaliseM ([LParam], Body)
internaliseStreamLambda internaliseLambda lam rowts = do
  chunk_size <- newVName "chunk_size"
  let chunk_param = I.Param chunk_size $ I.Prim int32
      chunktypes = map (`arrayOfRow` I.Var chunk_size) rowts
  (params, body, _) <- localScope (scopeOfLParams [chunk_param]) $
                       internaliseLambda lam chunktypes
  return (chunk_param:params, body)

-- Given @k@ lambdas, this will return a lambda that returns an
-- (k+2)-element tuple of integers.  The first element is the
-- equivalence class ID in the range [0,k].  The remaining are all zero
-- except for possibly one element.
internalisePartitionLambdas :: InternaliseLambda
                            -> [E.Exp]
                            -> [I.SubExp]
                            -> InternaliseM I.Lambda
internalisePartitionLambdas internaliseLambda lams args = do
  argtypes <- mapM I.subExpType args
  let rowtypes = map I.rowType argtypes
  lams' <- forM lams $ \lam -> do
    (params, body, _) <- internaliseLambda lam rowtypes
    return (params, body)
  params <- newIdents "partition_param" rowtypes
  let params' = [ I.Param name t
                | I.Ident name t <- params]
  body <- mkCombinedLambdaBody params 0 lams'
  return $ I.Lambda params' body rettype
  where k = length lams
        rettype = replicate (k+2) $ I.Prim int32
        result i = resultBody $
                   map constant $ (fromIntegral i :: Int32) :
                   (replicate i 0 ++ [1::Int32] ++ replicate (k-i) 0)
        mkCombinedLambdaBody :: [I.Ident]
                             -> Int
                             -> [([I.LParam], I.Body)]
                             -> InternaliseM I.Body
        mkCombinedLambdaBody _      i [] =
          return $ result i
        mkCombinedLambdaBody params i ((lam_params,lam_body):lams') =
          case lam_body of
            Body () bodybnds [boolres] -> do
              intres <- (:) <$> newIdent "eq_class" (I.Prim int32) <*>
                        replicateM (k+1) (newIdent "partition_incr" $ I.Prim int32)
              next_lam_body <-
                mkCombinedLambdaBody (map paramIdent lam_params) (i+1) lams'
              let parambnds =
                    [ mkLet' [] [paramIdent top] $ I.BasicOp $ I.SubExp $ I.Var $ I.identName fromp
                    | (top,fromp) <- zip lam_params params ]
                  branchbnd = mkLet' [] intres $ I.If boolres
                              (result i)
                              next_lam_body $
                              ifCommon rettype
              return $ mkBody (stmsFromList parambnds <> bodybnds <> oneStm branchbnd) $
                map (I.Var . I.identName) intres
            _ ->
              fail "Partition lambda returns too many values."
