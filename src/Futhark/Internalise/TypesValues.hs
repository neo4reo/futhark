{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Futhark.Internalise.TypesValues
  (
   -- * Internalising types
    BoundInTypes
  , boundInTypes
  , internaliseReturnType
  , internaliseEntryReturnType
  , internaliseParamTypes
  , internaliseType
  , internalisePrimType
  , internalisedTypeSize

  -- * Internalising values
  , internalisePrimValue
  , internaliseValue
  )
  where

import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Array as A
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import Data.Monoid ((<>))
import Data.Semigroup (Semigroup)

import qualified Language.Futhark as E
import Futhark.Representation.SOACS as I
import Futhark.Internalise.Monad

internaliseUniqueness :: E.Uniqueness -> I.Uniqueness
internaliseUniqueness E.Nonunique = I.Nonunique
internaliseUniqueness E.Unique = I.Unique

-- | The names that are bound for some types, either implicitly or
-- explicitly.
newtype BoundInTypes = BoundInTypes (S.Set VName)
                       deriving (Semigroup, Monoid)

-- | Determine the names bound for some types.
boundInTypes :: [E.TypeParam] -> BoundInTypes
boundInTypes = BoundInTypes . S.fromList . mapMaybe isTypeParam
  where isTypeParam (E.TypeParamDim v _) = Just v
        isTypeParam _ = Nothing

internaliseParamTypes :: BoundInTypes
                      -> M.Map VName VName
                      -> [E.TypeBase (E.DimDecl VName) ()]
                      -> InternaliseM ([[I.TypeBase ExtShape Uniqueness]],
                                       ConstParams)
internaliseParamTypes (BoundInTypes bound) pnames ts =
  runInternaliseTypeM $ withDims (bound' <> M.map (Free . Var) pnames) $
  mapM internaliseTypeM ts
  where bound' = M.fromList (zip (S.toList bound)
                                 (map (Free . Var) $ S.toList bound))

internaliseReturnType :: E.TypeBase (E.DimDecl VName) ()
                      -> InternaliseM ([I.TypeBase ExtShape Uniqueness],
                                       ConstParams)
internaliseReturnType t = do
  (ts', cm') <- internaliseEntryReturnType t
  return (concat ts', cm')

-- | As 'internaliseReturnType', but returns components of a top-level
-- tuple type piecemeal.
internaliseEntryReturnType :: E.TypeBase (E.DimDecl VName) ()
                           -> InternaliseM ([[I.TypeBase ExtShape Uniqueness]],
                                            ConstParams)
internaliseEntryReturnType t = do
  let ts = case E.isTupleRecord t of Just tts -> tts
                                     _        -> [t]
  runInternaliseTypeM $ mapM internaliseTypeM ts

internaliseType :: E.TypeBase () ()
                -> InternaliseM [I.TypeBase I.ExtShape Uniqueness]
internaliseType =
  fmap fst . runInternaliseTypeM . internaliseTypeM . E.vacuousShapeAnnotations

newId :: InternaliseTypeM Int
newId = do (i,cm) <- get
           put (i + 1, cm)
           return i

internaliseDim :: E.DimDecl VName
               -> InternaliseTypeM ExtSize
internaliseDim d =
  case d of
    E.AnyDim -> Ext <$> newId
    E.ConstDim n -> return $ Free $ intConst I.Int32 $ toInteger n
    E.NamedDim name -> namedDim name
  where namedDim (E.QualName _ name) = do
          subst <- liftInternaliseM $ asks $ M.lookup name . envSubsts
          is_dim <- lookupDim name
          case (is_dim, subst) of
            (Just dim, _) -> return dim
            (Nothing, Just [v]) -> return $ I.Free v
            _ -> do -- Then it must be a constant.
              let fname = nameFromString $ pretty name ++ "f"
              (i,cm) <- get
              case find ((==fname) . fst) cm of
                Just (_, known) -> return $ I.Free $ I.Var known
                Nothing -> do new <- liftInternaliseM $ newVName $ baseString name
                              put (i, (fname,new):cm)
                              return $ I.Free $ I.Var new

internaliseTypeM :: E.StructType
                 -> InternaliseTypeM [I.TypeBase ExtShape Uniqueness]
internaliseTypeM orig_t =
  case orig_t of
    E.Prim bt -> return [I.Prim $ internalisePrimType bt]
    E.TypeVar{} ->
      fail "internaliseTypeM: cannot handle type variable."
    E.Record ets ->
      concat <$> mapM (internaliseTypeM . snd) (E.sortFields ets)
    E.Array et shape u -> do
      dims <- internaliseShape shape
      ets <- internaliseElemType et
      return [I.arrayOf et' (Shape dims) $ internaliseUniqueness u | et' <- ets ]
    E.Arrow{} -> fail $ "internaliseTypeM: cannot handle function type: " ++ pretty orig_t

  where internaliseElemType E.ArrayPolyElem{} =
          fail "internaliseElemType: cannot handle type variable."
        internaliseElemType (E.ArrayPrimElem bt _) =
          return [I.Prim $ internalisePrimType bt]
        internaliseElemType (E.ArrayRecordElem elemts) =
          concat <$> mapM (internaliseRecordElem . snd) (E.sortFields elemts)

        internaliseRecordElem (E.RecordArrayElem et) =
          internaliseElemType et
        internaliseRecordElem (E.RecordArrayArrayElem et shape u) =
          internaliseTypeM $ E.Array et shape u

        internaliseShape = mapM internaliseDim . E.shapeDims

internaliseSimpleType :: E.TypeBase () ()
                      -> Maybe [I.TypeBase ExtShape NoUniqueness]
internaliseSimpleType = fmap (map I.fromDecl) . internaliseTypeWithUniqueness

internaliseTypeWithUniqueness :: E.TypeBase () ()
                              -> Maybe [I.TypeBase ExtShape Uniqueness]
internaliseTypeWithUniqueness = flip evalStateT 0 . internaliseType'
  where internaliseType' E.TypeVar{} =
          lift Nothing
        internaliseType' (E.Prim bt) =
          return [I.Prim $ internalisePrimType bt]
        internaliseType' (E.Record ets) =
          concat <$> mapM (internaliseType' . snd) (E.sortFields ets)
        internaliseType' (E.Array et shape u) = do
          dims <- map Ext <$> replicateM (E.shapeRank shape) newId'
          ets <- internaliseElemType et
          return [I.arrayOf et' (Shape dims) $ internaliseUniqueness u | et' <- ets ]
        internaliseType' E.Arrow{} =
          fail "internaliseTypeWithUniqueness: cannot handle function type."

        internaliseElemType E.ArrayPolyElem{} =
          lift Nothing
        internaliseElemType (E.ArrayPrimElem bt _) =
          return [I.Prim $ internalisePrimType bt]
        internaliseElemType (E.ArrayRecordElem elemts) =
          concat <$> mapM (internaliseRecordElem . snd) (E.sortFields elemts)

        internaliseRecordElem (E.RecordArrayElem et) =
          internaliseElemType et
        internaliseRecordElem (E.RecordArrayArrayElem et shape u) =
          internaliseType' $ E.Array et shape u

        newId' = do i <- get
                    put $ i + 1
                    return i

-- | How many core language values are needed to represent one source
-- language value of the given type?
internalisedTypeSize :: E.ArrayDim dim =>
                        E.TypeBase dim () -> InternaliseM Int
internalisedTypeSize = fmap length . internaliseType . E.removeShapeAnnotations

-- | Transform an external value to a number of internal values.
-- Roughly:
--
-- * The resulting list is empty if the original value is an empty
--   tuple.
--
-- * It contains a single element if the original value was a
-- singleton tuple or non-tuple.
--
-- * The list contains more than one element if the original value was
-- a non-empty non-singleton tuple.
--
-- Although note that the transformation from arrays-of-tuples to
-- tuples-of-arrays may also contribute to several discrete arrays
-- being returned for a single input array.
--
-- If the input value is or contains a non-regular array, 'Nothing'
-- will be returned.
internaliseValue :: E.Value -> Maybe [I.Value]
internaliseValue (E.ArrayValue arr rt) = do
  arrayvalues <- mapM internaliseValue $ A.elems arr
  ts <- internaliseSimpleType rt
  let arrayvalues' =
        case arrayvalues of
          [] -> replicate (length ts) []
          _  -> transpose arrayvalues
  zipWithM asarray ts arrayvalues'
  where asarray rt' values =
          let shape = determineShape (I.arrayRank rt') values
              values' = concatMap flat values
              size = product shape
          in if size == length values' then
               Just $ I.ArrayVal (A.listArray (0,size - 1) values')
               (I.elemType rt') shape
             else Nothing
        flat (I.PrimVal bv)      = [bv]
        flat (I.ArrayVal bvs _ _) = A.elems bvs
internaliseValue (E.PrimValue bv) =
  return [I.PrimVal $ internalisePrimValue bv]

determineShape :: Int -> [I.Value] -> [Int]
determineShape _ vs@(I.ArrayVal _ _ shape : _) =
  length vs : shape
determineShape r vs =
  length vs : replicate r 0

-- | Convert an external primitive to an internal primitive.
internalisePrimType :: E.PrimType -> I.PrimType
internalisePrimType (E.Signed t) = I.IntType t
internalisePrimType (E.Unsigned t) = I.IntType t
internalisePrimType (E.FloatType t) = I.FloatType t
internalisePrimType E.Bool = I.Bool

-- | Convert an external primitive value to an internal primitive value.
internalisePrimValue :: E.PrimValue -> I.PrimValue
internalisePrimValue (E.SignedValue v) = I.IntValue v
internalisePrimValue (E.UnsignedValue v) = I.IntValue v
internalisePrimValue (E.FloatValue v) = I.FloatValue v
internalisePrimValue (E.BoolValue b) = I.BoolValue b
