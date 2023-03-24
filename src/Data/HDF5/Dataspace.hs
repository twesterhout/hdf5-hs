{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.HDF5.Dataspace
  ( Dataspace (..)
  , createDataspace
  , createDataspaceForHyperslab
  , selectHyperslab
  , selectHyperslabWith
  , getSelection
  , getDataspaceShape
  , isScalarDataspace
  , Selection (..)
  , SelectionType (..)
  , Hyperslab (..)
  , rowMajorStrides
  , colMajorStrides
  , hyperslabForShapeStrides
  , hyperslabBoundingBox
  , h5s_close
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.GADT.Compare
import Data.GADT.Show
import Data.HDF5.Context
import Data.HDF5.Types
import Data.Some
import Data.Type.Equality
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as V
import Data.Vector.Storable.Mutable qualified as MV
import Foreign.C.Types
import Foreign.Ptr
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import GHC.Stack
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import UnliftIO.Foreign

C.context (C.baseCtx <> C.bsCtx <> C.funCtx <> h5Ctx)
C.include "<hdf5.h>"

newtype Dataspace s = Dataspace (Handle s)
  deriving stock (Generic)
  deriving anyclass (NFData)

instance HasField "rawHandle" (Dataspace s) Hid where
  getField (Dataspace (Handle h _)) = h
  {-# INLINE getField #-}

createDataspace :: (MonadUnliftIO m) => [Int] -> HDF5 s m (Dataspace s)
createDataspace [] = Dataspace <$> createHandle h5s_close [CU.exp| hid_t { H5Screate(H5S_SCALAR) } |]
createDataspace shape =
  fmap Dataspace $
    withArrayLen (fromIntegral <$> shape) $ \(fromIntegral -> rank) (c_shape :: Ptr Hsize) ->
      createHandle
        h5s_close
        [CU.exp| hid_t { H5Screate_simple($(int rank), $(const hsize_t* c_shape), $(const hsize_t* c_shape)) } |]

isScalarDataspace :: (MonadUnliftIO m) => Dataspace s -> HDF5 s m Bool
isScalarDataspace ((.rawHandle) -> h) = toBool <$> liftIO [CU.exp| bool { H5Sget_simple_extent_ndims($(hid_t h)) == 0 } |]

h5s_close :: (MonadIO m) => Hid -> m ()
h5s_close h = void . liftIO $ [CU.exp| herr_t { H5Sclose($(hid_t h)) } |]

data Hyperslab = Hyperslab
  { hyperslabStart :: {-# UNPACK #-} !(Vector Int)
  , hyperslabStride :: {-# UNPACK #-} !(Vector Int)
  , hyperslabCount :: {-# UNPACK #-} !(Vector Int)
  , hyperslabBlock :: {-# UNPACK #-} !(Vector Int)
  }
  deriving stock (Read, Show, Eq, Generic)
  deriving anyclass (NFData)

data SelectionType = SelectedNone | SelectedPoints | SelectedHyperslabs | SelectedAll
  deriving stock (Read, Show, Eq, Generic)
  deriving anyclass (NFData)

data SelectCombine = SelectSet | SelectOr | SelectAnd | SelectXor | SelectNotA | SelectNotB
  deriving stock (Read, Show, Eq, Generic)
  deriving anyclass (NFData)

data Selection (t :: SelectionType) where
  SelectionNone :: Selection 'SelectedNone
  SelectionPoints :: Selection 'SelectedPoints
  SelectionHyperslabs :: Maybe Hyperslab -> Selection 'SelectedHyperslabs
  SelectionAll :: Selection 'SelectedAll

deriving stock instance Show (Selection t)

deriving stock instance Eq (Selection t)

instance GEq Selection where
  geq SelectionNone SelectionNone = Just Refl
  geq SelectionPoints SelectionPoints = Just Refl
  geq (SelectionHyperslabs a) (SelectionHyperslabs b) = if a == b then Just Refl else Nothing
  geq SelectionAll SelectionAll = Just Refl
  geq _ _ = Nothing

instance GShow Selection where gshowsPrec = defaultGshowsPrec

getSelection :: (HasCallStack, MonadUnliftIO m) => Dataspace s -> HDF5 s m (Some Selection)
getSelection dspace@((.rawHandle) -> h) = do
  selectType <- liftIO [CU.exp| int { H5Sget_select_type($(hid_t h)) } |]
  let r
        | toBool [CU.pure| bool { $(int selectType) == H5S_SEL_NONE } |] = pure . mkSome $ SelectionNone
        | toBool [CU.pure| bool { $(int selectType) == H5S_SEL_POINTS } |] = pure . mkSome $ SelectionPoints
        | toBool [CU.pure| bool { $(int selectType) == H5S_SEL_HYPERSLABS } |] = do
            liftIO [CU.exp| htri_t { H5Sis_regular_hyperslab($(hid_t h)) } |] >>= fromHtri >>= \case
              True -> mkSome . SelectionHyperslabs . Just <$> unsafeGetHyperslab dspace
              False -> pure . mkSome $ SelectionHyperslabs Nothing
        | toBool [CU.pure| bool { $(int selectType) == H5S_SEL_ALL } |] = pure . mkSome $ SelectionAll
        | otherwise = error $ "invalid H5S_sel_type: " <> show selectType
  r

selectOperatorToCInt :: H5S_seloper_t -> CInt
selectOperatorToCInt = \case
  H5S_SELECT_SET -> [CU.pure| int { H5S_SELECT_SET } |]
  H5S_SELECT_OR -> [CU.pure| int { H5S_SELECT_OR } |]
  H5S_SELECT_AND -> [CU.pure| int { H5S_SELECT_AND } |]
  H5S_SELECT_XOR -> [CU.pure| int { H5S_SELECT_XOR } |]
  H5S_SELECT_NOTB -> [CU.pure| int { H5S_SELECT_NOTB } |]
  H5S_SELECT_NOTA -> [CU.pure| int { H5S_SELECT_NOTA } |]

selectHyperslab :: (MonadUnliftIO m) => Hyperslab -> Dataspace s -> HDF5 s m (Dataspace s)
selectHyperslab = selectHyperslabWith H5S_SELECT_SET

selectHyperslabWith :: (MonadUnliftIO m) => H5S_seloper_t -> Hyperslab -> Dataspace s -> HDF5 s m (Dataspace s)
selectHyperslabWith (selectOperatorToCInt -> combine) (Hyperslab start stride size block) ((.rawHandle) -> h) = do
  fmap Dataspace $
    createHandle h5s_close $
      V.unsafeWith (V.map fromIntegral start) $ \startPtr ->
        V.unsafeWith (V.map fromIntegral stride) $ \stridePtr ->
          V.unsafeWith (V.map fromIntegral size) $ \sizePtr ->
            V.unsafeWith (V.map fromIntegral block) $ \blockPtr ->
              [CU.block| hid_t {
                hid_t const copy = H5Scopy($(hid_t h));
                H5Sselect_hyperslab(copy, $(int combine),
                                    $(const hsize_t* startPtr),
                                    $(const hsize_t* stridePtr),
                                    $(const hsize_t* sizePtr),
                                    $(const hsize_t* blockPtr));
                return copy;
              } |]

getDataspaceRank :: (MonadUnliftIO m) => Dataspace s -> HDF5 s m Int
getDataspaceRank ((.rawHandle) -> h) =
  fromIntegral <$> liftIO [CU.exp| int { H5Sget_simple_extent_ndims($(hid_t h)) } |]

getDataspaceShape :: (HasCallStack, MonadUnliftIO m) => Dataspace s -> HDF5 s m (Maybe [Int])
getDataspaceShape dspace =
  getSelection dspace >>= \case
    Some SelectionAll -> Just <$> unsafeGetShape dspace
    Some (SelectionHyperslabs (Just h)) ->
      pure . Just . V.toList $ V.zipWith (*) h.hyperslabCount h.hyperslabBlock
    _ -> pure Nothing

unsafeGetShape :: (MonadUnliftIO m) => Dataspace s -> HDF5 s m [Int]
unsafeGetShape dspace@((.rawHandle) -> h) = do
  rank <- getDataspaceRank dspace
  allocaArray rank $ \shapePtr -> do
    _ <- liftIO [CU.exp| int { H5Sget_simple_extent_dims($(hid_t h), $(hsize_t* shapePtr), NULL) } |]
    fmap fromIntegral <$> peekArray rank shapePtr

unsafeGetHyperslab :: (MonadUnliftIO m) => Dataspace s -> HDF5 s m Hyperslab
unsafeGetHyperslab ((.rawHandle) -> h) = liftIO $ do
  rank <- fromIntegral <$> [CU.exp| int { H5Sget_simple_extent_ndims($(hid_t h)) } |]
  start <- MV.new rank
  stride <- MV.new rank
  count <- MV.new rank
  block <- MV.new rank
  _ <- MV.unsafeWith start $ \startPtr ->
    MV.unsafeWith stride $ \stridePtr ->
      MV.unsafeWith count $ \countPtr ->
        MV.unsafeWith block $ \blockPtr ->
          [CU.exp| herr_t {
            H5Sget_regular_hyperslab(
              $(hid_t h),
              $(hsize_t* startPtr),
              $(hsize_t* stridePtr),
              $(hsize_t* countPtr),
              $(hsize_t* blockPtr)) } |]
  start' <- V.unsafeFreeze start
  stride' <- V.unsafeFreeze stride
  count' <- V.unsafeFreeze count
  block' <- V.unsafeFreeze block
  pure $
    Hyperslab
      (V.map fromIntegral start')
      (V.map fromIntegral stride')
      (V.map fromIntegral count')
      (V.map fromIntegral block')

-- | Get strides corresponding to row-major ordering
rowMajorStrides
  :: Integral a
  => [a]
  -- ^ Extents
  -> [a]
rowMajorStrides = drop 1 . scanr (*) 1

-- | Get strides corresponding to column-major ordering.
colMajorStrides
  :: Integral a
  => [a]
  -- ^ Extents
  -> [a]
colMajorStrides = scanl (*) 1 . init

hyperslabForShapeStrides :: [Int] -> [Int] -> Hyperslab
hyperslabForShapeStrides shape strides
  | strides == rowMajorStrides shape =
      Hyperslab
        { hyperslabStart = V.replicate n 0
        , hyperslabStride = V.replicate n 1
        , hyperslabCount = V.fromListN n shape
        , hyperslabBlock = V.replicate n 1
        }
  | otherwise = error "not yet implemented"
  where
    n = length shape

hyperslabBoundingBox :: Hyperslab -> [Int]
hyperslabBoundingBox h =
  V.toList $
    V.zipWith4
      (\start stride count block -> start + count * block + (count - 1) * (stride - 1))
      h.hyperslabStart
      h.hyperslabStride
      h.hyperslabCount
      h.hyperslabBlock

createDataspaceForHyperslab :: (MonadUnliftIO m) => Hyperslab -> HDF5 s m (Dataspace s)
createDataspaceForHyperslab hyperslab = do
  dspace <- createDataspace (hyperslabBoundingBox hyperslab)
  isScalarDataspace dspace >>= \case
    True -> pure dspace
    False -> selectHyperslab hyperslab dspace
