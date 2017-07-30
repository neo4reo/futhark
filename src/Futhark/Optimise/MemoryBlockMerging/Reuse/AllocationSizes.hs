{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Find all Alloc statements and associate their memory blocks with the
-- allocation size.
module Futhark.Optimise.MemoryBlockMerging.Reuse.AllocationSizes
  ( memBlockSizesFunDef, memBlockSizesParamsBodyNonRec
  , Sizes
  ) where

import qualified Data.Map.Strict as M
import Control.Monad.Writer

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory
  (ExplicitMemorish, ExplicitMemory, InKernel)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Representation.Kernels.Kernel

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous


type Sizes = M.Map VName SubExp

newtype FindM lore a = FindM { unFindM :: Writer Sizes a }
  deriving (Monad, Functor, Applicative,
            MonadWriter Sizes)

type LoreConstraints lore = (ExplicitMemorish lore,
                             AllocSizeUtils lore,
                             FullWalk lore)

recordMapping :: VName -> SubExp -> FindM lore ()
recordMapping var size = tell $ M.singleton var size

coerce :: (ExplicitMemorish flore, ExplicitMemorish tlore) =>
          FindM flore a -> FindM tlore a
coerce = FindM . unFindM

memBlockSizesFunDef :: LoreConstraints lore =>
                       FunDef lore -> Sizes
memBlockSizesFunDef fundef =
  let m = unFindM $ do
        mapM_ lookInFParam $ funDefParams fundef
        lookInBody $ funDefBody fundef
      mem_sizes = execWriter m
  in mem_sizes

memBlockSizesParamsBodyNonRec :: LoreConstraints lore =>
                           [FParam lore] -> Body lore -> Sizes
memBlockSizesParamsBodyNonRec params body =
  let m = unFindM $ do
        mapM_ lookInFParam params
        mapM_ lookInStm $ bodyStms body
      mem_sizes = execWriter m
  in mem_sizes

lookInFParam :: LoreConstraints lore =>
                FParam lore -> FindM lore ()
lookInFParam (Param mem (ExpMem.MemMem size _space)) =
  recordMapping mem size
lookInFParam _ = return ()

lookInLParam :: LoreConstraints lore =>
                LParam lore -> FindM lore ()
lookInLParam (Param mem (ExpMem.MemMem size _space)) =
  recordMapping mem size
lookInLParam _ = return ()

lookInBody :: LoreConstraints lore =>
              Body lore -> FindM lore ()
lookInBody (Body _ bnds _res) =
  mapM_ lookInStmRec bnds

lookInKernelBody :: LoreConstraints lore =>
                    KernelBody lore -> FindM lore ()
lookInKernelBody (KernelBody _ bnds _res) =
  mapM_ lookInStmRec bnds

lookInStm :: LoreConstraints lore =>
             Stm lore -> FindM lore ()
lookInStm (Let (Pattern patctxelems patvalelems) _ e) = do
  case patvalelems of
    [PatElem mem _ _] ->
      case lookForAllocSize e of
        Just size ->
          recordMapping mem size
        Nothing -> return ()
    _ -> return ()
  mapM_ lookInPatCtxElem patctxelems

lookInStmRec :: LoreConstraints lore =>
             Stm lore -> FindM lore ()
lookInStmRec stm@(Let _ _ e) = do
  lookInStm stm

  fullWalkExpM walker walker_kernel e
  where walker = identityWalker
          { walkOnBody = lookInBody
          , walkOnFParam = lookInFParam
          , walkOnLParam = lookInLParam
          }
        walker_kernel = identityKernelWalker
          { walkOnKernelBody = coerce . lookInBody
          , walkOnKernelKernelBody = coerce . lookInKernelBody
          , walkOnKernelLParam = lookInLParam
          }

lookInPatCtxElem :: LoreConstraints lore =>
                    PatElem lore -> FindM lore ()
lookInPatCtxElem (PatElem mem _bindage (ExpMem.MemMem size _)) =
  recordMapping mem size
lookInPatCtxElem _ = return ()


-- FIXME: Clean this up.
class AllocSizeUtils lore where
  lookForAllocSize :: Exp lore -> Maybe SubExp

instance AllocSizeUtils ExplicitMemory where
  lookForAllocSize (Op (ExpMem.Alloc size _)) = Just size
  lookForAllocSize _ = Nothing

instance AllocSizeUtils InKernel where
  lookForAllocSize (Op (ExpMem.Alloc size _)) = Just size
  lookForAllocSize _ = Nothing