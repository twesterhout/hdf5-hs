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
  ( withFile,
    withFile',
    AccessFlags (..),
    makeGroup,
    withGroup,
    withObject,
    readDataset,
    writeDataset,
    readAttribute,
    writeAttribute,
    exists,
    existsAttribute,
    delete,
    deleteAttribute,
  )
where

import Control.Exception.Safe hiding (handle)
import Data.ByteString (packCString, useAsCString)
import Data.Complex
import Data.Constraint (Dict (..))
import Data.HDF5.Types
import Data.HDF5.Wrapper
import Data.Some
import qualified Data.Text as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.C.String (peekCString)
import Foreign.C.Types (CChar)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (allocaArray, peekArray, withArrayLen)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable (..))
import qualified GHC.TypeLits
import System.Directory (doesFileExist)
import System.Process (callProcess)
import Prelude hiding (find, first, group, withFile)

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

-- | [bracket](https://wiki.haskell.org/Bracket_pattern) for HDF5 files.
withFile ::
  (HasCallStack, MonadIO m, MonadMask m) =>
  -- | filename
  Text ->
  -- | mode in which to open the file
  AccessFlags ->
  -- | action to perform
  (File -> m a) ->
  m a
withFile path flags = bracket (liftIO acquire) (\(File h) -> liftIO (h5f_close h))
  where
    acquire = fmap File $
      case flags of
        ReadOnly -> h5f_open path flags
        WriteTruncate -> h5f_create path flags
        WriteAppend ->
          doesFileExist (toString path) >>= \case
            True -> h5f_open path flags
            False -> h5f_create path flags

withFile' ::
  (HasCallStack, MonadIO m, MonadMask m) =>
  -- | filename
  Text ->
  -- | mode in which to open the file
  AccessFlags ->
  -- | action to perform
  (Group -> m a) ->
  m a
withFile' path flags action = withFile path flags $ \file ->
  forceGroup file action

makeGroup :: (HasCallStack, MonadIO m) => Group -> Text -> m ()
makeGroup parent path = liftIO $ h5g_create (getRawHandle parent) path

class IsIndex i where
  withObject :: (HasCallStack, MonadIO m, MonadMask m) => Group -> i -> (forall t. Object t -> m a) -> m a

withObjectRaw :: (HasCallStack, MonadIO m) => Hid -> (forall t. Object t -> m a) -> m a
withObjectRaw h action = do
  liftIO (h5i_get_type h) >>= \case
    H5I_FILE -> action (File h)
    H5I_GROUP -> action (Group h)
    H5I_DATASET -> action (Dataset h)
    H5I_DATATYPE -> action (Datatype h)
    t -> do
      name <- liftIO $ h5i_get_name h
      error $ show name <> " is a " <> show t <> "; expected an Object"

-- instance IsIndex String where
--   withObject parent path action = withObject parent (toText path) action

instance IsIndex String where
  withObject parent path action = withObject parent (toText path) action

instance IsIndex Text where
  withObject parent path action =
    bracket (liftIO $ h5o_open (getRawHandle parent) path) (liftIO . h5o_close) $ \h ->
      withObjectRaw h action

instance IsIndex Int where
  withObject parent index action =
    bracket (liftIO $ h5o_open_by_idx (getRawHandle parent) index) (liftIO . h5o_close) $ \h ->
      withObjectRaw h action

forceGroup :: (HasCallStack, MonadIO m, MonadMask m) => Object t -> (Group -> m a) -> m a
forceGroup !object !action = case object of
  (File h) -> bracket (liftIO $ h5o_open h "/") (liftIO . h5o_close) $ \h' -> action (Group h')
  (Group h) -> action (Group h)
  _ -> getName object >>= \name -> error $ show name <> " is not a Group"

forceDataset :: (HasCallStack, MonadIO m) => Object t -> (Dataset -> m a) -> m a
forceDataset object action = case object of
  (Dataset h) -> action (Dataset h)
  _ -> getName object >>= \name -> error $ show name <> " is not a Dataset"

withGroup :: (HasCallStack, IsIndex i, MonadIO m, MonadMask m) => Group -> i -> (Group -> m a) -> m a
withGroup !parent !index !action =
  withObject parent index $ \object -> forceGroup object action

getName :: (HasCallStack, MonadIO m) => Object t -> m Text
getName object = liftIO $ h5i_get_name (getRawHandle object)

readDataset :: (HasCallStack, IsIndex i, KnownDataset a, MonadIO m, MonadMask m) => Group -> i -> m a
readDataset parent index = withObject parent index $ \object ->
  forceDataset object $ \(Dataset h) -> liftIO (h5d_read h)

writeDataset :: (HasCallStack, i ~ Text, KnownDataset a, MonadIO m, MonadMask m) => Group -> i -> a -> m ()
writeDataset parent index value = do
  exists parent index >>= \case
    False -> do
      liftIO $
        withArrayView value $ \(ArrayView (Datatype object_dtype) (Dataspace object_dspace) ~_) ->
          h5d_create (getRawHandle parent) index object_dtype object_dspace >>= h5o_close
    _ -> return ()
  let action (Dataset h) = liftIO (h5d_write h value)
  withObject parent index $ \object -> forceDataset object action

delete :: (HasCallStack, MonadIO m, MonadMask m, IsIndex i) => Group -> i -> m ()
delete parent index = withObject parent index $ \object -> do
  name <- getName object
  liftIO $ h5l_delete (getRawHandle parent) name

deleteAttribute :: (HasCallStack, IsIndex i, MonadIO m, MonadMask m) => Group -> i -> Text -> m ()
deleteAttribute parent index name = withObject parent index $ \object ->
  liftIO $ h5a_delete (getRawHandle object) name

withRoot :: (HasCallStack, MonadIO m, MonadMask m) => Object t -> (Group -> m a) -> m a
withRoot object action =
  bracket (liftIO $ h5i_get_file_id (getRawHandle object)) (liftIO . h5f_close) $ \f ->
    forceGroup (File f) action

exists :: forall m. (HasCallStack, MonadIO m, MonadMask m) => Group -> Text -> m Bool
exists parent path
  | T.null path = return True
  | T.head path == '/' = withRoot parent $ \g -> exists g (T.tail path)
  | otherwise = liftIO $ existsHelper (getRawHandle parent) (T.split (== '/') path)
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

existsAttribute :: (HasCallStack, IsIndex i, MonadIO m, MonadMask m) => Group -> i -> Text -> m Bool
existsAttribute parent index name = withObject parent index $ \object ->
  liftIO $ h5a_exists (getRawHandle object) name

readAttribute ::
  (HasCallStack, IsIndex i, KnownDatatype a, MonadIO m, MonadMask m) =>
  Group ->
  i ->
  Text ->
  m a
readAttribute parent index name = withObject parent index $ \object ->
  liftIO $ h5a_read (getRawHandle object) name

writeAttribute ::
  (HasCallStack, IsIndex i, KnownDatatype a, MonadIO m, MonadMask m) =>
  Group ->
  i ->
  Text ->
  a ->
  m ()
writeAttribute parent index name value = withObject parent index $ \object ->
  liftIO $ h5a_write (getRawHandle object) name value

-- writeDataset :: (HasCallStack, MonadIO m, MonadMask m, KnownDataset a, IsIndex i) => Group -> i -> a -> m ()
-- writeDataset parent index value = withObject parent index $ \object ->
--   forceDataset object $ \(Dataset h) -> liftIO (h5d_write h value)

getRawHandle :: Object t -> Hid
getRawHandle (File h) = h
getRawHandle (Group h) = h
getRawHandle (Dataset h) = h
getRawHandle (Datatype h) = h
