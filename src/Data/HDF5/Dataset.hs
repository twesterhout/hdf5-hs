{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.HDF5.Dataset
  ( Dataset
  , KnownDataset (..)
  , Allocatable (..)
  , getDatatype
  , getDataspace
  , createDataset
  , writeDataset
  , writeDatasetExplicit
  , readDataset
  , readDatasetInto
  , readDatasetExplicit
  , createUnfilledDataset
  )
where

import Control.Monad (forM, unless, void)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class
import Data.ByteString qualified as BS
import Data.Coerce
import Data.HDF5.Context
import Data.HDF5.Dataspace
import Data.HDF5.Datatype
import Data.HDF5.Group
import Data.HDF5.Object
import Data.HDF5.Types
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as V
import Foreign.C.Types
import Foreign.Ptr
import GHC.Stack
import GHC.TypeLits
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import UnliftIO.Foreign

C.context (C.baseCtx <> C.bsCtx <> C.funCtx <> h5Ctx)
C.include "<hdf5.h>"

class (KnownDatatype (ElementOf a)) => KnownDataset a where
  withPtrShapeStride :: MonadUnliftIO m => a -> (Ptr (ElementOf a) -> [Int] -> [Int] -> m b) -> m b

class (KnownDatatype (ElementOf a), MonadUnliftIO m) => Allocatable m a where
  fromPtrShape :: ForeignPtr (ElementOf a) -> [Int] -> m a

type instance ElementOf (Scalar a) = a

instance (KnownDatatype a, Storable a) => KnownDataset (Scalar a) where
  withPtrShapeStride (Scalar x) action =
    with x $ \ptr ->
      action ptr [] []

instance (MonadUnliftIO m, KnownDatatype a, Storable a) => Allocatable m (Scalar a) where
  fromPtrShape fp [] = Scalar <$> withForeignPtr fp (liftIO . peek)
  fromPtrShape _ shape = error $ "wrong shape: " <> show shape <> "; expected []"

type instance ElementOf (Vector a) = a

instance (KnownDatatype a, Storable a) => KnownDataset (Vector a) where
  withPtrShapeStride (V.unsafeToForeignPtr0 -> (fp, n)) action =
    withForeignPtr fp $ \ptr ->
      action ptr [n] [1]

class (KnownDatatype (ElementOf a)) => ListKnownDataset (n :: Nat) a where
  listWithPtrShapeStride :: MonadUnliftIO m => a -> (Ptr (ElementOf a) -> [Int] -> [Int] -> m b) -> m b
  listFromPtrShape :: MonadUnliftIO m => ForeignPtr (ElementOf a) -> [Int] -> m a

type family ListElement (a :: Type) where
  ListElement [[a]] = ListElement [a]
  ListElement [a] = a

type family ListDimension (a :: Type) :: Nat where
  ListDimension [[a]] = 1 + ListDimension [a]
  ListDimension [a] = 1

type instance ElementOf [a] = ListElement [a]

instance (ElementOf [a] ~ a, KnownDatatype a, Storable a) => ListKnownDataset 1 [a] where
  listWithPtrShapeStride arr action =
    withArrayLen arr $ \n ptr -> action ptr [n] [1]
  listFromPtrShape fp [d0] =
    withForeignPtr fp $ \ptr0 ->
      forM [0 .. d0 - 1] $ \i0 ->
        liftIO (peekElemOff ptr0 i0)
  listFromPtrShape _ _ = error "this should never happen"

instance (ElementOf [[a]] ~ a, KnownDatatype a, Storable a) => ListKnownDataset 2 [[a]] where
  listWithPtrShapeStride arr action = do
    let d0 = length arr
        d1 = if d0 == 0 then 0 else length (head arr)
        shape = [d0, d1]
        flat = concat arr
    withArrayLen flat $ \totalSize ptr -> do
      unless (totalSize == product shape) $
        error "list doesn't have a regular shape (i.e. rows have varying number of elements)"
      action ptr shape (rowMajorStrides shape)
  listFromPtrShape fp [d0, d1] =
    withForeignPtr fp $ \ptr0 ->
      forM [0 .. d0 - 1] $ \i0 ->
        forM [0 .. d1 - 1] $ \i1 ->
          liftIO $ peekElemOff ptr0 (i0 * d1 + i1)
  listFromPtrShape _ _ = error "this should never happen"

instance (ElementOf [[[a]]] ~ a, KnownDatatype a, Storable a) => ListKnownDataset 3 [[[a]]] where
  listWithPtrShapeStride arr action = do
    let d0 = length arr
        d1 = if d0 == 0 then 0 else length (head arr)
        d2 = if d1 == 0 then 0 else length (head (head arr))
        shape = [d0, d1, d2]
        flat = concatMap concat arr
    withArrayLen flat $ \totalSize ptr -> do
      unless (totalSize == product shape) $
        error "list doesn't have a regular shape (i.e. rows have varying number of elements)"
      action ptr shape (rowMajorStrides shape)
  listFromPtrShape fp [d0, d1, d2] =
    withForeignPtr fp $ \ptr0 ->
      forM [0 .. d0 - 1] $ \i0 ->
        forM [0 .. d1 - 1] $ \i1 ->
          forM [0 .. d2 - 1] $ \i2 ->
            liftIO $ peekElemOff ptr0 (i0 * d1 * d2 + i1 * d2 + i2)
  listFromPtrShape _ _ = error "this should never happen"

instance (ElementOf [[[[a]]]] ~ a, KnownDatatype a, Storable a) => ListKnownDataset 4 [[[[a]]]] where
  listWithPtrShapeStride arr action = do
    let d0 = length arr
        d1 = if d0 == 0 then 0 else length (head arr)
        d2 = if d1 == 0 then 0 else length (head (head arr))
        d3 = if d2 == 0 then 0 else length (head (head (head arr)))
        shape = [d0, d1, d2, d3]
        flat = concat . concat . concat $ arr
    withArrayLen flat $ \totalSize ptr -> do
      unless (totalSize == product shape) $
        error "list doesn't have a regular shape (i.e. rows have varying number of elements)"
      action ptr shape (rowMajorStrides shape)
  listFromPtrShape fp [d0, d1, d2, d3] =
    withForeignPtr fp $ \ptr0 ->
      forM [0 .. d0 - 1] $ \i0 ->
        forM [0 .. d1 - 1] $ \i1 ->
          forM [0 .. d2 - 1] $ \i2 ->
            forM [0 .. d3 - 1] $ \i3 ->
              liftIO $ peekElemOff ptr0 (i0 * d1 * d2 * d3 + i1 * d2 * d3 + i2 * d3 + i3)
  listFromPtrShape _ _ = error "this should never happen"

instance (ListKnownDataset (ListDimension [a]) [a]) => KnownDataset [a] where
  withPtrShapeStride = listWithPtrShapeStride @(ListDimension [a]) @[a]

instance (MonadUnliftIO m, ListKnownDataset (ListDimension [a]) [a]) => Allocatable m [a] where
  fromPtrShape = listFromPtrShape @(ListDimension [a]) @[a]

type instance ElementOf BS.ByteString = BS.ByteString

useAsCString :: MonadUnliftIO m => BS.ByteString -> (CString -> m a) -> m a
useAsCString str action = do
  runInIO <- askRunInIO
  liftIO $ BS.useAsCString str (runInIO . action)

instance KnownDataset BS.ByteString where
  withPtrShapeStride str action =
    alloca $ \stringPtr ->
      useAsCString str $ \charPtr -> do
        liftIO (poke stringPtr charPtr)
        action (castPtr stringPtr) [] []

instance MonadUnliftIO m => Allocatable m BS.ByteString where
  fromPtrShape fp [] =
    withForeignPtr fp $ \(castPtr -> stringPtr) ->
      liftIO $ peek stringPtr >>= BS.packCString
  fromPtrShape _ shape = error $ "wrong shape: " <> show shape <> "; expected []"

type instance ElementOf Text = Text

instance KnownDataset Text where
  withPtrShapeStride str action =
    withPtrShapeStride (encodeUtf8 str) $ \ptr shape strides ->
      action (castPtr ptr) shape strides

instance MonadUnliftIO m => Allocatable m Text where
  fromPtrShape ptr shape = decodeUtf8 <$> fromPtrShape (coerce ptr) shape

getDatatype :: (MonadUnliftIO m) => Dataset s -> HDF5 s m (Datatype s)
getDatatype ((.rawHandle) -> dataset) = do
  Datatype <$> createHandle h5o_close [CU.exp| hid_t { H5Dget_type($(hid_t dataset)) } |]

getDataspace :: (MonadUnliftIO m) => Dataset s -> HDF5 s m (Dataspace s)
getDataspace ((.rawHandle) -> dataset) = do
  Dataspace <$> createHandle h5s_close [CU.exp| hid_t { H5Dget_space($(hid_t dataset)) } |]

createUnfilledDataset :: (MonadUnliftIO m) => Group s -> Text -> Datatype s -> Dataspace s -> HDF5 s m (Dataset s)
createUnfilledDataset ((.rawHandle) -> parent) (encodeUtf8 -> name) ((.rawHandle) -> dtype) ((.rawHandle) -> dspace) =
  Dataset
    <$> createHandle
      h5o_close
      [C.exp| hid_t { H5Dcreate($(hid_t parent), $bs-cstr:name, $(hid_t dtype), $(hid_t dspace),
                                H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) } |]

createDataset :: forall a m s. (HasCallStack, MonadUnliftIO m, KnownDataset a) => Group s -> Text -> a -> HDF5 s m (Dataset s)
createDataset parent name object = do
  dtype <- createDatatype @(ElementOf a)
  withPtrShapeStride object $ \ptr shape strides -> do
    dspace <- createDataspaceForHyperslab (hyperslabForShapeStrides shape strides)
    dset <- createUnfilledDataset parent name dtype dspace
    writeDatasetExplicit dset dspace ptr shape strides
    pure dset

writeDatasetExplicit
  :: forall a m s
   . (HasCallStack, MonadUnliftIO m, KnownDatatype a)
  => Dataset s
  -> Dataspace s
  -> Ptr a
  -> [Int]
  -> [Int]
  -> HDF5 s m ()
writeDatasetExplicit dataset@((.rawHandle) -> c_dataset) ((.rawHandle) -> c_file_space) (castPtr -> c_buf) shape strides = do
  fileDType <- getDatatype dataset
  memDType@((.rawHandle) -> c_mem_dtype) <- createDatatype @a
  unless (fileDType == memDType) $
    error $
      "datatype mismatch: you are trying to write "
        <> show memDType
        <> " to a dataset containing "
        <> show fileDType
  ((.rawHandle) -> c_mem_space) <- createDataspaceForHyperslab (hyperslabForShapeStrides shape strides)
  void $
    liftIO
      [CU.exp| herr_t { H5Dwrite($(hid_t c_dataset), $(hid_t c_mem_dtype),
                                 $(hid_t c_mem_space), $(hid_t c_file_space),
                                 H5P_DEFAULT, $(const void* c_buf)) } |]

writeDataset
  :: forall a m s
   . (HasCallStack, MonadUnliftIO m, KnownDataset a)
  => a
  -> Dataset s
  -> HDF5 s m ()
writeDataset object dataset = do
  dataspace <- getDataspace dataset
  withPtrShapeStride object $ \ptr shape strides ->
    writeDatasetExplicit dataset dataspace ptr shape strides

readDatasetExplicit
  :: forall a m s
   . (HasCallStack, MonadUnliftIO m, KnownDatatype a)
  => Dataset s
  -> Dataspace s
  -> Ptr a
  -> [Int]
  -> [Int]
  -> HDF5 s m ()
readDatasetExplicit dataset@((.rawHandle) -> c_dset) ((.rawHandle) -> c_file_space) (castPtr -> dataPtr) shape strides = do
  fileDType <- getDatatype dataset
  memDType@((.rawHandle) -> c_mem_type) <- createDatatype @a
  unless (fileDType == memDType) $
    error $
      "datatype mismatch: you are trying to read "
        <> show memDType
        <> " from a dataset containing "
        <> show fileDType
  ((.rawHandle) -> c_mem_space) <- createDataspaceForHyperslab (hyperslabForShapeStrides shape strides)
  void $
    liftIO
      [C.exp| herr_t { H5Dread($(hid_t c_dset), $(hid_t c_mem_type), $(hid_t c_mem_space),
                               $(hid_t c_file_space), H5P_DEFAULT, $(void* dataPtr)) } |]

readDatasetInto
  :: forall a m s
   . (HasCallStack, MonadUnliftIO m, KnownDataset a)
  => a
  -> Dataset s
  -> HDF5 s m ()
readDatasetInto object dataset = do
  dataspace <- getDataspace dataset
  withPtrShapeStride object $ \dataPtr shape strides ->
    readDatasetExplicit dataset dataspace dataPtr shape strides

readDataset
  :: forall a m s
   . (KnownDataset a, Allocatable m a)
  => Dataset s
  -> HDF5 s m a
readDataset dataset = do
  dspace <- getDataspace dataset
  getDataspaceShape dspace >>= \case
    Just shape -> do
      dtype <- createDatatype @(ElementOf a)
      elementSizeBytes <- getDatatypeSize dtype
      let !totalSizeBytes = elementSizeBytes * product shape
      fp <- mallocForeignPtrBytes totalSizeBytes
      withForeignPtr fp $ \dataPtr ->
        readDatasetExplicit dataset dspace dataPtr shape (rowMajorStrides shape)
      lift $ fromPtrShape fp shape
    Nothing -> error "reading of non regularly shaped datasets is not supported"
