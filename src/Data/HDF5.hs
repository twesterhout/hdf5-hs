{-# LANGUAGE NoImplicitPrelude #-}

module Data.HDF5
  ( h5Open,
    h5Close,
    h5Name,
    h5Size,
    h5OpenByIndex,
    h5FoldlM,
    h5FindDataset,
    h5GetNumDims,
    h5GetDatasetInfo,
    h5ReadDataset,
    H5Handle (..),
    H5Exception (..),
    H5ConcreteBlob (..),
    H5Blob (..),
    toConcreteBlob,
    pattern H5FloatBlob,
  )
where

import Control.Exception.Safe hiding (handle)
import Data.HDF5.Internal
import Data.Typeable
import Data.Vector.Storable (Vector)
import Foreign.C.String (peekCString)
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (Storable (..))
import Relude
import System.Directory (doesFileExist)

data H5Handle
  = H5File !Hid
  | H5Group !Hid
  deriving stock (Eq, Show)

unHandle :: H5Handle -> Hid
unHandle = \case
  H5File h -> h
  H5Group h -> h

data H5Exception = H5Exception !Hid !(Maybe Text)
  deriving stock (Eq, Show, Generic)

instance Exception H5Exception

data H5ConcreteBlob a = H5ConcreteBlob {-# UNPACK #-} !(Vector a) ![Int]
  deriving stock (Show)

data H5Blob where
  H5Blob :: Typeable a => H5ConcreteBlob a -> H5Blob

toConcreteBlob :: Typeable a => H5Blob -> Maybe (H5ConcreteBlob a)
toConcreteBlob (H5Blob x) = cast x

pattern H5FloatBlob v n <- (toConcreteBlob @Float -> Just (H5ConcreteBlob v n))


data H5DatasetInfo = H5DatasetInfo ![Int] !H5T_class_t !Int
  deriving stock (Eq, Show)

h5Check :: (Integral a, MonadThrow m) => Maybe Text -> a -> m a
h5Check msg !code = (when (code < 0) . throwIO) (H5Exception code' msg) >> return code
  where
    code' = fromIntegral code

-- h5Check :: (MonadIO m, MonadFail m) => Maybe String -> Hid -> m Hid
-- h5Check msg code
--   | code < 0 = fail $ fromMaybe ("failed with error code " <> show code) msg
--   | otherwise = return code

h5Open :: (MonadIO m, MonadThrow m) => FilePath -> IOMode -> m H5Handle
h5Open path mode = do
  r <-
    liftIO $
      doesFileExist path >>= \case
        True ->
          -- TODO: Handle truncation properly!
          let flags = if mode == ReadMode then H5F_ACC_RDONLY else H5F_ACC_RDWR
           in h5f_open path flags H5P_DEFAULT
        False -> do
          when (mode == ReadMode || mode == AppendMode) . throwIO . H5Exception (-1) . Just $
            "file " <> show path <> " not found"
          h5f_create path H5F_ACC_EXCL H5P_DEFAULT H5P_DEFAULT
  H5File <$> h5Check (Just $ "error opening " <> show path) r

h5Close :: MonadIO m => H5Handle -> m ()
h5Close h = void . liftIO $ case h of
  H5File handle -> h5Check (Just "error closing file") =<< h5f_close handle
  H5Group handle -> h5Check (Just "error closing group") =<< h5o_close handle

checkValidHandle :: (MonadIO m, MonadThrow m) => H5Handle -> m ()
checkValidHandle handle
  | h < 0 = error $ "handle is invalid: " <> show handle
  | otherwise =
    (liftIO $ h5i_is_valid h) >>= \status -> case compare status 0 of
      LT -> throwIO $ H5Exception (fromIntegral status) Nothing
      EQ -> throwIO . H5Exception (-1) . Just $ "handle is invalid: " <> show handle
      _ -> return ()
  where
    h = unHandle handle

h5Size :: (MonadIO m, MonadThrow m) => H5Handle -> m Int
h5Size g = do
  checkValidHandle g
  r <- liftIO $ h5g_get_num_objs (unHandle g)
  h5Check (Just "error getting group size") r

h5Name :: (MonadIO m, MonadThrow m) => H5Handle -> m Text
h5Name handle =
  let h = unHandle handle
   in checkValidHandle handle >> do
        liftIO (h5i_get_name h nullPtr 0) >>= \r -> case compare r 0 of
          GT -> fmap toText . liftIO $
            allocaBytes (r + 1) $ \s ->
              h5i_get_name h s (r + 1) >>= h5Check msg >> peekCString s
          EQ -> return ""
          LT -> throwIO $ H5Exception (fromIntegral r) msg
  where
    msg = Just "error getting object's name"

h5OpenByIndex :: (MonadIO m, MonadThrow m) => H5Handle -> Int -> m H5Handle
h5OpenByIndex parent index =
  checkValidHandle parent >> do
    groupSize <- h5Size parent
    when (index < 0 || index >= groupSize) . error $ "invalid index: " <> show index
    r <- liftIO $ h5o_open_by_idx (unHandle parent) "." H5_INDEX_NAME H5_ITER_INC index H5P_DEFAULT
    H5Group <$> h5Check (Just $ "error accessing object at index " <> show index) r

h5ByIndex :: (MonadIO m, MonadMask m) => H5Handle -> Int -> (H5Handle -> m b) -> m b
h5ByIndex parent index = bracket (h5OpenByIndex parent index) h5Close

h5FoldlM :: (MonadIO m, MonadMask m) => (a -> H5Handle -> m a) -> a -> H5Handle -> m a
h5FoldlM f acc0 parent = go 0 acc0
  where
    go !i !acc =
      h5Size parent >>= \n ->
        if i < n
          then h5ByIndex parent i (f acc) >>= go (i + 1)
          else return acc

h5CreateGroup :: (MonadIO m, MonadThrow m) => H5Handle -> Text -> m H5Handle
h5CreateGroup parent path =
  fmap H5Group $
    checkValidHandle parent >> create parent path >>= h5Check msg
  where
    create h p =
      liftIO $
        h5g_create (unHandle h) (toString p) H5P_DEFAULT H5P_DEFAULT H5P_DEFAULT
    msg = Just $ "error creating group " <> show path

h5Delete :: (MonadIO m, MonadThrow m) => H5Handle -> Text -> m ()
h5Delete parent path = void $ checkValidHandle parent >> delete parent path >>= h5Check msg
  where
    delete h p = liftIO $ h5l_delete (unHandle h) (toString p) H5P_DEFAULT
    msg = Just $ "error deleting " <> show path

h5FindDataset :: (MonadIO m, MonadThrow m) => H5Handle -> Text -> m Bool
h5FindDataset group name =
  fmap (toEnum . fromIntegral) $
    checkValidHandle group >> find group name >>= h5Check msg
  where
    find h p = liftIO $ h5lt_find_dataset (unHandle h) (toString p)
    msg = Just $ "error searching for dataset " <> show name

h5GetNumDims :: (MonadIO m, MonadThrow m) => H5Handle -> Text -> m Int
h5GetNumDims parent path = do
  checkValidHandle parent
  fmap fromIntegral . liftIO . alloca $ \rankPtr ->
    getNumDims rankPtr >>= h5Check msg >> peek rankPtr
  where
    getNumDims ptr = h5lt_get_dataset_ndims (unHandle parent) (toString path) ptr
    msg = Just $ "failed to get number of dimensions of " <> show path

h5GetDatasetInfo :: (MonadIO m, MonadThrow m) => H5Handle -> Text -> m H5DatasetInfo
h5GetDatasetInfo parent path = do
  numDims <- h5GetNumDims parent path
  liftIO $
    allocaArray numDims $ \dimsPtr ->
      alloca $ \classIdPtr ->
        alloca $ \typeSizePtr -> do
          h5Check msg =<< getInfo dimsPtr classIdPtr typeSizePtr
          H5DatasetInfo
            <$> (fmap fromIntegral <$> peekArray numDims dimsPtr)
            <*> (toEnum . fromIntegral <$> peek classIdPtr)
            <*> (fromIntegral <$> peek typeSizePtr)
  where
    getInfo = h5lt_get_dataset_info (unHandle parent) (toString path)
    msg = Nothing

h5ReadDataset :: (MonadIO m, MonadThrow m) => H5Handle -> Text -> m H5Blob
h5ReadDataset = undefined
