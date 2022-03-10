{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Copyright: (c) 2020-2021 Tom Westerhout
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Tom Westerhout <14264576+twesterhout@users.noreply.github.com>
--
-- [HDF5](https://www.hdfgroup.org/solutions/hdf5) is a file format commonly
-- used for scientific data. It is especially great for storing large datasets
-- with lots of arrays or other structured data. This package provides a
-- high-level interface to [HDF5
-- library](https://portal.hdfgroup.org/pages/viewpage.action?pageId=50073943)
-- for Haskell programming language.
module Data.HDF5
  ( File,
    withFile,
    withFile',
    AccessFlags (..),
    type Data.HDF5.Types.Group,
    pattern Data.HDF5.Group,
    createGroup,
    groupSize,
    mapGroupM,
    forGroupM,
    -- openGroup,
    -- forceGroup,
    Dataset,
    -- openDataset,
    -- openObject,
    -- forceDataset,
    -- readDataset,
    -- writeDataset,
    -- createDataset,
    -- getDataspace,
    -- getHyperslab,
    -- sliceHyperslab,
    -- selectHyperslab,
    slice,
    sliceDataset,
    sliceWithHyperslab,
    Hyperslab,
    open,
    openByIndex,
    -- SelectionType (..),
    -- DatasetSlice (..),
    ArrayView' (..),
    Scalar (..),
    readInplace,
    readSelectedInplace,
    readDataset,
    readSelected,
    writeDataset,
    writeSelected,
    createDataset,
    createEmptyDataset,
    datasetRank,
    datasetShape,
    -- dataspaceSelectionType,
    Datatype,
    Dataspace,
    ofShape,
    -- ofType,
    Attribute,
    readAttribute,
    writeAttribute,
    existsAttribute,
    deleteAttribute,
    attributeDatatype,
    attributeDataspace,
    getName,
    exists,
    close,
    delete,
    H5Exception (..),
    KnownDatatype (..),
    -- KnownDataset (..),
    ElementOf,
    KnownDataset' (..),
    TemporaryContiguousArray (..),
    TemporaryStridedMatrix (..),
    -- ArrayView (..),
    MonadResource,
    disableDiagOutput,
  )
where

import Control.Exception.Safe hiding (handle)
import Control.Monad.IO.Unlift
-- import Control.Monad.Trans.Resource

import Data.Coerce
-- import Data.Constraint
import Data.HDF5.Types
import Data.HDF5.Wrapper
import Data.Some
import qualified Data.Text as T
import Data.Typeable (eqT, (:~:) (..))
import System.Directory (doesFileExist)
import qualified System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Resource
-- import qualified Unsafe.Coerce
import Prelude hiding (Handle, find, first, group, withFile)

-- $setup
-- >>> import qualified Data.HDF5 as H5
-- >>> import Data.HDF5 (IOMode(..))

-- | Run external @h5dump@ command with given arguments.
--
-- /Note:/ you need @hdf5-tools@ package for this function to work. It is mainly
-- used to testing.
-- dump ::
--   -- | Arguments to @h5dump@
--   [Text] ->
--   -- | Path to HDF5 file
--   Text ->
--   IO ()
-- dump args path = callProcess "h5dump" (toString <$> args <> [path])
pattern Group <- (Data.HDF5.Types.Group _)

-- | [bracket](https://wiki.haskell.org/Bracket_pattern) for HDF5 files.
openFile ::
  (HasCallStack, MonadResource m) =>
  -- | filename
  Text ->
  -- | mode in which to open the file
  AccessFlags ->
  -- | file handle
  m File
openFile path flags = do
  let acquire = case flags of
        ReadOnly -> h5f_open path flags
        WriteTruncate -> h5f_create path flags
        WriteAppend ->
          doesFileExist (toString path) >>= \case
            True -> h5f_open path flags
            False -> h5f_create path flags
  allocate (liftIO acquire) h5f_close >>= \case
    (k, v) -> return $ File (Handle v k)

-- | [bracket](https://wiki.haskell.org/Bracket_pattern) for HDF5 files.
withFile' ::
  (HasCallStack, MonadUnliftIO m, NFData a) =>
  -- | filename
  Text ->
  -- | mode in which to open the file
  AccessFlags ->
  -- | action to perform
  (File -> ResourceT m a) ->
  m a
withFile' path flags action = runHDF5 $ openFile path flags >>= action

withFile ::
  (HasCallStack, MonadUnliftIO m, NFData a) =>
  -- | filename
  Text ->
  -- | mode in which to open the file
  AccessFlags ->
  -- | action to perform
  (Group -> ResourceT m a) ->
  m a
withFile path flags action = withFile' path flags $ getRoot >=> action

-- createGroup :: (HasCallStack, MonadIO m) => Group -> Text -> m ()
-- createGroup parent path = liftIO $ h5g_create (rawHandle parent) path

-- class IsIndex i where
--   openObject :: (HasCallStack, MonadResource m) => Group -> i -> m (Some Object)

-- cast :: Object t1 -> Object t2
-- cast x = case

-- instance IsIndex String where
--   withObject parent path action = withObject parent (toText path) action

-- instance IsIndex Text where
--   openObject parent path = do
--     allocate (liftIO $ h5o_open (rawHandle parent) path) (liftIO . h5o_close) >>= \case
--       (k, v) -> openObjectRaw (Handle v k)

-- instance IsIndex Int where
--   openObject parent index =
--     allocate (liftIO $ h5o_open_by_idx (rawHandle parent) index) (liftIO . h5o_close) >>= \case
--       (k, v) -> openObjectRaw (Handle v k)

-- forceGroup :: (HasCallStack, MonadResource m) => Object t -> m Group
-- forceGroup object = case object of
--   (File _) ->
--     allocate (liftIO $ h5o_open (rawHandle object) "/") (liftIO . h5o_close) >>= \case
--       (k, v) -> return (Group (Handle v k))
--   (Group h) -> return (Group h)
--   _ -> error $ show (getName object) <> " is not a Group"

-- forceDataset :: (HasCallStack, MonadResource m) => Object t -> m Dataset
-- forceDataset object = case object of
--   (Dataset h) -> return (Dataset h)
--   _ -> error $ show (getName object) <> " is not a Dataset"

-- openGroup :: (HasCallStack, IsIndex i, MonadResource m) => Group -> i -> m Group
-- openGroup parent index = openObject parent index >>= foldSome forceGroup

-- openDataset :: (HasCallStack, IsIndex i, MonadResource m) => Group -> i -> m Dataset
-- openDataset parent index = openObject parent index >>= foldSome forceDataset

-- getName :: HasCallStack => Object t -> Text
-- getName object = System.IO.Unsafe.unsafePerformIO $ h5i_get_name (rawHandle object)

-- readDataset :: forall a m. (HasCallStack, KnownDataset a, MonadResource m) => Dataset -> m a
-- readDataset dataset = h5d_read dataset

-- forceDataset object $ \(Dataset h) -> liftIO (h5d_read h)

ofShape :: MonadResource m => [Int] -> m Dataspace
ofShape shape = simpleDataspace shape

-- (a -> m b) -> m a -> m b
-- (a -> b -> m c) -> m a -> m b -> m c
-- (a -> b -> c -> m d) -> m a -> m b -> m c -> m d
--
-- m (a -> m b) -> m a -> m b
--
-- op f x = do
--    f' <- f
--    x' <- x
--    f' x'
--
-- op f x = do
--    f' <- f
--    x' <- x
--    return (f' x')
--
-- m (a -> b -> m c) -> m a -> m (b -> m c)
-- m (a -> b -> c -> m d) -> m a -> m (b -> c -> m d)

-- createDataset :: MonadIO m => Group -> Text -> Dataspace -> Datatype -> m ()
-- createDataset parent index dspace dtype =
--   liftIO $ h5d_create (rawHandle parent) index dtype dspace >>= h5o_close

createDataset ::
  forall a m.
  (HasCallStack, KnownDataset' a, MonadResource m) =>
  Group ->
  Text ->
  a ->
  m ()
createDataset parent path object = do
  alreadyThere <- exists parent path
  unless alreadyThere $
    withArrayView' object $ \view -> do
      memDatatype <- ofType @(ElementOf a)
      memDataspace <- arrayViewDataspace view
      fileDataspace <- ofShape (dataspaceShape memDataspace)
      -- trace (show (dataspaceShape fileDataspace)) $ pure ()
      dataset <- createEmptyDataset parent path memDatatype fileDataspace
      -- These are purely an optimization
      close dataset
      close fileDataspace
      close memDataspace
      close memDatatype
  open parent path >>= writeDataset object

-- writeDataset :: (HasCallStack, KnownDataset a, MonadResource m) => Group -> Text -> a -> m ()
-- writeDataset parent index value =
--   withArrayView value $ \view@(ArrayView object_dtype object_dspace _) -> do
--     exists parent index >>= \case
--       False -> do
--         dataset_dspace <- guessDataspace object_dspace
--         createDataset parent index dataset_dspace object_dtype
--       _ -> return ()
--     openObject parent index >>= foldSome forceDataset >>= \object -> h5d_write object view

delete :: (HasCallStack, MonadIO m) => Group -> Text -> m ()
delete parent name = liftIO $ h5l_delete (rawHandle parent) name

getRoot :: (HasCallStack, MonadResource m) => Object t -> m Group
getRoot object =
  allocate (liftIO $ h5i_get_file_id (rawHandle object)) (liftIO . h5f_close) >>= \case
    (k, v) -> pure $ Data.HDF5.Types.Group (Handle v k)

exists :: forall m. (HasCallStack, MonadResource m) => Group -> Text -> m Bool
exists parent path
  | T.null path = return True
  | T.head path == '/' = getRoot parent >>= \g -> exists g (T.tail path)
  | otherwise = liftIO $ existsHelper (rawHandle parent) (T.split (== '/') path)
  where
    existsHelper :: Hid -> [Text] -> IO Bool
    existsHelper _ [] = return True
    existsHelper p (first : rest) = do
      h5l_exists p first >>= \case
        True -> bracket (h5o_open p first) h5o_close $ \o ->
          h5i_get_type o >>= \case
            H5I_GROUP -> existsHelper o rest
            _ -> return $ null rest
        False -> return False

-- readAttribute ::
--   (HasCallStack, KnownDataset a, MonadResource m) =>
--   Object t ->
--   Text ->
--   m a
-- readAttribute object name = h5a_read (rawHandle object) name

-- writeAttribute ::
--   (HasCallStack, KnownDataset a, MonadResource m) =>
--   Object t ->
--   Text ->
--   a ->
--   m ()
-- writeAttribute object name value = h5a_write (rawHandle object) name value
