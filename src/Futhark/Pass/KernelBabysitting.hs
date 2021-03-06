{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Do various kernel optimisations - mostly related to coalescing.
module Futhark.Pass.KernelBabysitting
       ( babysitKernels
       , nonlinearInMemory
       )
       where

import Control.Arrow (first)
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Semigroup ((<>))

import Futhark.MonadFreshNames
import Futhark.Representation.AST
import Futhark.Representation.Kernels
       hiding (Prog, Body, Stm, Pattern, PatElem,
               BasicOp, Exp, Lambda, FunDef, FParam, LParam, RetType)
import Futhark.Tools
import Futhark.Pass
import Futhark.Util

babysitKernels :: Pass Kernels Kernels
babysitKernels = Pass "babysit kernels"
                 "Transpose kernel input arrays for better performance." $
                 intraproceduralTransformation transformFunDef

transformFunDef :: MonadFreshNames m => FunDef Kernels -> m (FunDef Kernels)
transformFunDef fundec = do
  (body', _) <- modifyNameSource $ runState (runBinderT m M.empty)
  return fundec { funDefBody = body' }
  where m = inScopeOf fundec $
            transformBody mempty $ funDefBody fundec

type BabysitM = Binder Kernels

transformBody :: ExpMap -> Body Kernels -> BabysitM (Body Kernels)
transformBody expmap (Body () bnds res) = insertStmsM $ do
  foldM_ transformStm expmap bnds
  return $ resultBody res

-- | Map from variable names to defining expression.  We use this to
-- hackily determine whether something is transposed or otherwise
-- funky in memory (and we'd prefer it not to be).  If we cannot find
-- it in the map, we just assume it's all good.  HACK and FIXME, I
-- suppose.  We really should do this at the memory level.
type ExpMap = M.Map VName (Stm Kernels)

nonlinearInMemory :: VName -> ExpMap -> Maybe (Maybe [Int])
nonlinearInMemory name m =
  case M.lookup name m of
    Just (Let _ _ (BasicOp (Rearrange perm _))) -> Just $ Just perm
    Just (Let _ _ (BasicOp (Reshape _ arr))) -> nonlinearInMemory arr m
    Just (Let _ _ (BasicOp (Manifest perm _))) -> Just $ Just perm
    Just (Let pat _ (Op (Kernel _ _ ts _))) ->
      nonlinear =<< find ((==name) . patElemName . fst)
      (zip (patternElements pat) ts)
    _ -> Nothing
  where nonlinear (pe, t)
          | inner_r <- arrayRank t, inner_r > 0 = do
              let outer_r = arrayRank (patElemType pe) - inner_r
              return $ Just $ [inner_r..inner_r+outer_r-1] ++ [0..inner_r-1]
          | otherwise = Nothing

transformStm :: ExpMap -> Stm Kernels -> BabysitM ExpMap

transformStm expmap (Let pat aux (Op (Kernel desc space ts kbody))) = do
  -- Go spelunking for accesses to arrays that are defined outside the
  -- kernel body and where the indices are kernel thread indices.
  scope <- askScope
  let thread_gids = map fst $ spaceDimensions space
      thread_local = S.fromList $ spaceGlobalId space : spaceLocalId space : thread_gids

  kbody'' <- evalStateT (traverseKernelBodyArrayIndexes
                         thread_local
                         (castScope scope <> scopeOfKernelSpace space)
                         (ensureCoalescedAccess expmap (spaceDimensions space) num_threads)
                         kbody)
             mempty

  let bnd' = Let pat aux $ Op $ Kernel desc space ts kbody''
  addStm bnd'
  return $ M.fromList [ (name, bnd') | name <- patternNames pat ] <> expmap
  where num_threads = spaceNumThreads space

transformStm expmap (Let pat aux e) = do
  e' <- mapExpM (transform expmap) e
  let bnd' = Let pat aux e'
  addStm bnd'
  return $ M.fromList [ (name, bnd') | name <- patternNames pat ] <> expmap

transform :: ExpMap -> Mapper Kernels Kernels BabysitM
transform expmap =
  identityMapper { mapOnBody = \scope -> localScope scope . transformBody expmap }

type ArrayIndexTransform m =
  (VName -> Bool) ->           -- thread local?
  (SubExp -> Maybe SubExp) ->  -- split substitution?
  Scope InKernel ->            -- type environment
  VName -> Slice SubExp -> m (Maybe (VName, Slice SubExp))

traverseKernelBodyArrayIndexes :: (Applicative f, Monad f) =>
                                  Names
                               -> Scope InKernel
                               -> ArrayIndexTransform f
                               -> KernelBody InKernel
                               -> f (KernelBody InKernel)
traverseKernelBodyArrayIndexes thread_variant outer_scope f (KernelBody () kstms kres) =
  KernelBody () . stmsFromList <$>
  mapM (onStm (varianceInStms mempty kstms,
               mkSizeSubsts kstms,
               outer_scope)) (stmsToList kstms) <*>
  pure kres
  where onLambda (variance, szsubst, scope) lam =
          (\body' -> lam { lambdaBody = body' }) <$>
          onBody (variance, szsubst, scope') (lambdaBody lam)
          where scope' = scope <> scopeOfLParams (lambdaParams lam)

        onStreamLambda (variance, szsubst, scope) lam =
          (\body' -> lam { groupStreamLambdaBody = body' }) <$>
          onBody (variance, szsubst, scope') (groupStreamLambdaBody lam)
          where scope' = scope <> scopeOf lam

        onBody (variance, szsubst, scope) (Body battr stms bres) = do
          stms' <- stmsFromList <$> mapM (onStm (variance', szsubst', scope')) (stmsToList stms)
          Body battr stms' <$> pure bres
          where variance' = varianceInStms variance stms
                szsubst' = mkSizeSubsts stms <> szsubst
                scope' = scope <> scopeOf stms

        onStm (variance, szsubst, _) (Let pat attr (BasicOp (Index arr is))) =
          Let pat attr . oldOrNew <$> f isThreadLocal sizeSubst outer_scope arr is
          where oldOrNew Nothing =
                  BasicOp $ Index arr is
                oldOrNew (Just (arr', is')) =
                  BasicOp $ Index arr' is'

                isThreadLocal v =
                  not $ S.null $
                  thread_variant `S.intersection`
                  M.findWithDefault (S.singleton v) v variance

                sizeSubst (Constant v) = Just $ Constant v
                sizeSubst (Var v)
                  | v `M.member` outer_scope      = Just $ Var v
                  | Just v' <- M.lookup v szsubst = sizeSubst v'
                  | otherwise                      = Nothing

        onStm (variance, szsubst, scope) (Let pat attr e) =
          Let pat attr <$> mapExpM (mapper (variance, szsubst, scope)) e

        mapper ctx = identityMapper { mapOnBody = const (onBody ctx)
                                    , mapOnOp = onOp ctx
                                    }

        onOp ctx (GroupReduce w lam input) =
          GroupReduce w <$> onLambda ctx lam <*> pure input
        onOp ctx (GroupStream w maxchunk lam accs arrs) =
           GroupStream w maxchunk <$> onStreamLambda ctx lam <*> pure accs <*> pure arrs
        onOp _ stm = pure stm

        mkSizeSubsts = fold . fmap mkStmSizeSubst
          where mkStmSizeSubst (Let (Pattern [] [pe]) _ (Op (SplitSpace _ _ _ elems_per_i))) =
                  M.singleton (patElemName pe) elems_per_i
                mkStmSizeSubst _ = mempty

-- Not a hashmap, as SubExp is not hashable.
type Replacements = M.Map (VName, Slice SubExp) VName

ensureCoalescedAccess :: (MonadBinder m, Lore m ~ Kernels) =>
                         ExpMap
                      -> [(VName,SubExp)]
                      -> SubExp
                      -> ArrayIndexTransform (StateT Replacements m)
ensureCoalescedAccess expmap thread_space num_threads isThreadLocal sizeSubst outer_scope arr slice = do
  seen <- gets $ M.lookup (arr, slice)

  case (seen, isThreadLocal arr, typeOf <$> M.lookup arr outer_scope) of
    -- Already took care of this case elsewhere.
    (Just arr', _, _) ->
      pure $ Just (arr', slice)

    (Nothing, False, Just t)
      -- We are fully indexing the array with thread IDs, but the
      -- indices are in a permuted order.
      | Just is <- sliceIndices slice,
        length is == arrayRank t,
        Just is' <- coalescedIndexes (map Var thread_gids) is,
        Just perm <- is' `isPermutationOf` is ->
          replace =<< lift (rearrangeInput (nonlinearInMemory arr expmap) perm arr)

      -- We are not fully indexing the array, and the indices are not
      -- a proper prefix of the thread indices, and some indices are
      -- thread local, so we assume (HEURISTIC!)  that the remaining
      -- dimensions will be traversed sequentially.
      | (is, rem_slice) <- splitSlice slice,
        not $ null rem_slice,
        not $ tooSmallSlice (primByteSize (elemType t)) rem_slice,
        is /= map Var (take (length is) thread_gids) || length is == length thread_gids,
        any isThreadLocal (S.toList $ freeIn is) -> do
          let perm = coalescingPermutation (length is) $ arrayRank t
          replace =<< lift (rearrangeInput (nonlinearInMemory arr expmap) perm arr)

      -- We are taking a slice of the array with a unit stride.  We
      -- assume that the slice will be traversed sequentially.
      --
      -- We will really want to treat the sliced dimension like two
      -- dimensions so we can transpose them.  This may require
      -- padding.
      | (is, rem_slice) <- splitSlice slice,
        and $ zipWith (==) is $ map Var thread_gids,
        DimSlice offset len (Constant stride):_ <- rem_slice,
        isThreadLocalSubExp offset,
        Just {} <- sizeSubst len,
        oneIsh stride -> do
          let num_chunks = if null is
                           then primExpFromSubExp int32 num_threads
                           else coerceIntPrimExp Int32 $
                                product $ map (primExpFromSubExp int32) $
                                drop (length is) thread_gdims
          replace =<< lift (rearrangeSlice (length is) (arraySize (length is) t) num_chunks arr)

      -- Everything is fine... assuming that the array is in row-major
      -- order!  Make sure that is the case.
      | Just{} <- nonlinearInMemory arr expmap ->
          case sliceIndices slice of
            Just is | Just _ <- coalescedIndexes (map Var thread_gids) is ->
                        replace =<< lift (rowMajorArray arr)
                    | otherwise ->
                        return Nothing
            _ -> replace =<< lift (rowMajorArray arr)

    _ -> return Nothing

  where (thread_gids, thread_gdims) = unzip thread_space

        replace arr' = do
          modify $ M.insert (arr, slice) arr'
          return $ Just (arr', slice)

        isThreadLocalSubExp (Var v) = isThreadLocal v
        isThreadLocalSubExp Constant{} = False

-- Heuristic for avoiding rearranging too small arrays.
tooSmallSlice :: Int32 -> Slice SubExp -> Bool
tooSmallSlice bs = fst . foldl comb (True,bs) . sliceDims
  where comb (True, x) (Constant (IntValue (Int32Value d))) = (d*x < 4, d*x)
        comb (_, x)     _                                   = (False, x)

splitSlice :: Slice SubExp -> ([SubExp], Slice SubExp)
splitSlice [] = ([], [])
splitSlice (DimFix i:is) = first (i:) $ splitSlice is
splitSlice is = ([], is)

-- Try to move thread indexes into their proper position.
coalescedIndexes :: [SubExp] -> [SubExp] -> Maybe [SubExp]
coalescedIndexes tgids is
  -- Do Nothing if:
  -- 1. the innermost index is the innermost thread id
  --    (because access is already coalesced)
  -- 2. any of the indices is a constant, i.e., kernel free variable
  --    (because it would transpose a bigger array then needed -- big overhead).
  | any isCt is =
      Nothing
  | num_is > 0 && not (null tgids) && last is == last tgids =
      Just is
  -- Otherwise try fix coalescing
  | otherwise =
      Just $ reverse $ foldl move (reverse is) $ zip [0..] (reverse tgids)
  where num_is = length is

        move is_rev (i, tgid)
          -- If tgid is in is_rev anywhere but at position i, and
          -- position i exists, we move it to position i instead.
          | Just j <- elemIndex tgid is_rev, i /= j, i < num_is =
              swap i j is_rev
          | otherwise =
              is_rev

        swap i j l
          | Just ix <- maybeNth i l,
            Just jx <- maybeNth j l =
              update i jx $ update j ix l
          | otherwise =
              error $ "coalescedIndexes swap: invalid indices" ++ show (i, j, l)

        update 0 x (_:ys) = x : ys
        update i x (y:ys) = y : update (i-1) x ys
        update _ _ []     = error "coalescedIndexes: update"

        isCt :: SubExp -> Bool
        isCt (Constant _) = True
        isCt (Var      _) = False

coalescingPermutation :: Int -> Int -> [Int]
coalescingPermutation num_is rank =
  [num_is..rank-1] ++ [0..num_is-1]

rearrangeInput :: MonadBinder m =>
                  Maybe (Maybe [Int]) -> [Int] -> VName -> m VName
rearrangeInput (Just (Just current_perm)) perm arr
  | current_perm == perm = return arr -- Already has desired representation.
rearrangeInput Nothing perm arr
  | sort perm == perm = return arr -- We don't know the current
                                   -- representation, but the indexing
                                   -- is linear, so let's hope the
                                   -- array is too.
rearrangeInput (Just Just{}) perm arr
  | sort perm == perm = rowMajorArray arr -- We just want a row-major array, no tricks.
rearrangeInput manifest perm arr = do
  -- We may first manifest the array to ensure that it is flat in
  -- memory.  This is sometimes unnecessary, in which case the copy
  -- will hopefully be removed by the simplifier.
  manifested <- if isJust manifest then rowMajorArray arr else return arr
  letExp (baseString arr ++ "_coalesced") $
    BasicOp $ Manifest perm manifested

rowMajorArray :: MonadBinder m =>
                 VName -> m VName
rowMajorArray arr = do
  rank <- arrayRank <$> lookupType arr
  letExp (baseString arr ++ "_rowmajor") $ BasicOp $ Manifest [0..rank-1] arr

rearrangeSlice :: MonadBinder m =>
                  Int -> SubExp -> PrimExp VName -> VName
               -> m VName
rearrangeSlice d w num_chunks arr = do
  num_chunks' <- letSubExp "num_chunks" =<< toExp num_chunks

  (w_padded, padding) <- paddedScanReduceInput w num_chunks'

  per_chunk <- letSubExp "per_chunk" $ BasicOp $ BinOp (SQuot Int32) w_padded num_chunks'
  arr_t <- lookupType arr
  arr_padded <- padArray w_padded padding arr_t
  rearrange num_chunks' w_padded per_chunk (baseString arr) arr_padded arr_t

  where padArray w_padded padding arr_t = do
          let arr_shape = arrayShape arr_t
              padding_shape = setDim d arr_shape padding
          arr_padding <-
            letExp (baseString arr <> "_padding") $
            BasicOp $ Scratch (elemType arr_t) (shapeDims padding_shape)
          letExp (baseString arr <> "_padded") $
            BasicOp $ Concat d arr [arr_padding] w_padded

        rearrange num_chunks' w_padded per_chunk arr_name arr_padded arr_t = do
          let arr_dims = arrayDims arr_t
              pre_dims = take d arr_dims
              post_dims = drop (d+1) arr_dims
              extradim_shape = Shape $ pre_dims ++ [num_chunks', per_chunk] ++ post_dims
              tr_perm = [0..d-1] ++ map (+d) ([1] ++ [2..shapeRank extradim_shape-1-d] ++ [0])
          arr_extradim <-
            letExp (arr_name <> "_extradim") $
            BasicOp $ Reshape (map DimNew $ shapeDims extradim_shape) arr_padded
          arr_extradim_tr <-
            letExp (arr_name <> "_extradim_tr") $
            BasicOp $ Manifest tr_perm arr_extradim
          arr_inv_tr <- letExp (arr_name <> "_inv_tr") $
            BasicOp $ Reshape (map DimCoercion pre_dims ++ map DimNew (w_padded : post_dims))
            arr_extradim_tr
          letExp (arr_name <> "_inv_tr_init") =<<
            eSliceArray d  arr_inv_tr (eSubExp $ constant (0::Int32)) (eSubExp w)

paddedScanReduceInput :: MonadBinder m =>
                         SubExp -> SubExp
                      -> m (SubExp, SubExp)
paddedScanReduceInput w stride = do
  w_padded <- letSubExp "padded_size" =<<
              eRoundToMultipleOf Int32 (eSubExp w) (eSubExp stride)
  padding <- letSubExp "padding" $ BasicOp $ BinOp (Sub Int32) w_padded w
  return (w_padded, padding)

--- Computing variance.

type VarianceTable = M.Map VName Names

varianceInStms :: VarianceTable -> Stms InKernel -> VarianceTable
varianceInStms t = foldl varianceInStm t . stmsToList

varianceInStm :: VarianceTable -> Stm InKernel -> VarianceTable
varianceInStm variance bnd =
  foldl' add variance $ patternNames $ stmPattern bnd
  where add variance' v = M.insert v binding_variance variance'
        look variance' v = S.insert v $ M.findWithDefault mempty v variance'
        binding_variance = mconcat $ map (look variance) $ S.toList (freeInStm bnd)
