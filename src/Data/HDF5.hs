{-# LANGUAGE NoImplicitPrelude #-}

module Data.HDF5
  ( withFile,
    getName,
    getSize,
    getDims,
    byIndex,
    byName,
    foldM,
    Object (..),
    Some (..),
    makeGroup,
    delete,
    -- h5Name,
    -- h5Size,
    -- h5OpenByIndex,
    -- h5FoldlM,
    -- h5FindDataset,
    -- h5GetNumDims,
    -- h5GetDatasetInfo,
    -- readDataset,
    -- readTypedDataset,
    readDataset,
    readDataset',
    makeDataset,
    disableDiagOutput,
    -- H5Handle (..),
    H5Exception (..),
    -- H5ConcreteBlob (..),
    -- H5Blob (..),
    -- toConcreteBlob,
    -- pattern H5Int16Blob,
    -- pattern H5Int32Blob,
    -- pattern H5Int64Blob,
    -- pattern H5FloatBlob,
    -- pattern H5DoubleBlob,
    -- H5NativeDatatype (..),
  )
where

import Control.Exception.Safe hiding (handle)
import Data.HDF5.Internal
-- import Data.Proxy

import Data.Some
import Data.Typeable (cast)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.C.String (peekCString)
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (allocaArray, peekArray, withArrayLen)
import Foreign.Ptr (Ptr, castPtr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.Storable (Storable (..))
import qualified GHC.Show
import Relude hiding (find, group, withFile)
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)

-- import Type.Reflection

-- data H5Handle
--   = H5File !Hid
--   | H5Group !Hid
--   deriving stock (Eq, Show)

-- unHandle :: H5Handle -> Hid
-- unHandle = \case
--   H5File h -> h
--   H5Group h -> h
-- {-# INLINE unHandle #-}

-- ** Error Handling

data H5Exception = H5Exception !Hid ![ErrorInfo] !(Maybe Text)
  deriving stock (Generic)

instance Exception H5Exception

instance Show H5Exception where
  show = toString . prettyH5Exception

prettyH5Exception :: H5Exception -> Text
prettyH5Exception (H5Exception code stack msg) = "HDF5 error " <> show code <> msg' <> stack'
  where
    msg' = case msg of
      Just s -> ": " <> s
      Nothing -> ""
    stack' = case (intersperse "\n  " $ prettyErrorInfo <$> stack) of
      xs@(_ : _) -> mconcat $ "\n  " : xs
      [] -> ""

prettyErrorInfo :: ErrorInfo -> Text
prettyErrorInfo info =
  foldr (<>) "" $
    [ errorInfoFile info,
      ":",
      show (errorInfoLine info),
      ": in ",
      errorInfoFunc info,
      "()"
    ]
      ++ desc
  where
    desc = case errorInfoDesc info of
      Just msg -> [": ", msg]
      Nothing -> []

collectStack :: MonadIO m => m [ErrorInfo]
collectStack = liftIO . mask_ $
  bracket acquire release $ \stackId -> do
    (listRef :: IORef [ErrorInfo]) <- newIORef []
    let callback _ ptr _ = peek ptr >>= \info -> modifyIORef listRef ((:) info) >> return 0
    bracket (mkWalk callback) freeHaskellFunPtr $ \fn -> do
      status <- h5e_walk stackId H5E_WALK_UPWARD fn nullPtr
      when (status < 0) . error $ "H5Ewalk2 failed with error code " <> show status
    readIORef listRef
  where
    acquire = mask_ $ do
      h <- h5e_get_current_stack
      when (h < 0) . error $ "H5Eget_current_stack failed with error code " <> show h
      return h
    release h = mask_ $ do
      status <- h5e_close_stack h
      when (status < 0) . error $ "H5Eclose_stack failed with error code " <> show status
    foo = undefined

disableDiagOutput :: MonadIO m => m ()
disableDiagOutput = do
  status <- liftIO $ h5e_set_auto {-H5E_DEFAULT-} 0 nullFunPtr nullPtr
  when (status < 0) . error $ "H5Eset_auto2 failed with error code " <> show status

h5Fail :: (Integral a, MonadIO m, MonadThrow m) => Maybe Text -> a -> m b
h5Fail !msg !code = collectStack >>= \stack -> throw (H5Exception (fromIntegral code) stack msg)

h5Check :: (Integral a, MonadIO m, MonadThrow m) => Maybe Text -> a -> m a
h5Check msg !code = when (code < 0) (h5Fail msg code) >> return code

-- ** Files

openFile :: (MonadIO m, MonadThrow m) => FilePath -> IOMode -> m File
openFile path mode = do
  r <-
    liftIO $
      doesFileExist path >>= \case
        True -> case mode of
          ReadMode -> h5f_open path H5F_ACC_RDONLY H5P_DEFAULT
          WriteMode -> h5f_create path H5F_ACC_TRUNC H5P_DEFAULT H5P_DEFAULT
          _ -> h5f_open path H5F_ACC_RDWR H5P_DEFAULT
        False -> case mode of
          ReadMode -> fileNotFound
          _ -> h5f_create path H5F_ACC_EXCL H5P_DEFAULT H5P_DEFAULT
  File <$> h5Check (Just $ "error opening file " <> show path) r
  where
    fileNotFound = throw . H5Exception (-1) [] . Just $ "file " <> show path <> " not found"

-- closeHandle :: (MonadIO m, MonadThrow m) => H5Handle -> m ()
-- closeHandle h = void $ case h of
--   H5File handle -> h5Check (Just "error closing file") =<< liftIO (h5f_close handle)
--   H5Group handle -> h5Check (Just "error closing group") =<< liftIO (h5o_close handle)

withFile :: (MonadIO m, MonadMask m) => FilePath -> IOMode -> (File -> m a) -> m a
withFile path mode = bracket (openFile path mode) close

-- ** Groups

-- h5Size :: (MonadIO m, MonadThrow m) => H5Handle -> m Int
-- h5Size g = do
--   checkValidHandle g
--   r <- liftIO $ h5g_get_num_objs (unHandle g)
--   h5Check (Just "error getting group size") r

getSize :: (MonadIO m, MonadThrow m) => Object t -> m Int
getSize = h5Check (Just "error getting group size") <=< liftIO . h5g_get_num_objs . getRawHandle

getName :: (MonadIO m, MonadThrow m) => Object t -> m Text
getName object = do
  liftIO (h5i_get_name h nullPtr 0) >>= \r -> case compare r 0 of
    GT -> fmap toText . liftIO $
      allocaBytes (r + 1) $ \s ->
        h5i_get_name h s (r + 1) >>= h5Check msg >> peekCString s
    EQ -> return ""
    LT -> throw $ H5Exception (fromIntegral r) [] msg
  where
    !h = getRawHandle object
    msg = Just "error getting object name"

_constructObject :: (MonadIO m, MonadMask m) => Hid -> m (Some Object)
_constructObject h =
  flip onException (closeObjectImpl h) $
    _getObjectType h >>= \case
      H5O_TYPE_GROUP -> return . Some . Group $ h
      H5O_TYPE_DATASET -> return . Some . Dataset $ h
      H5O_TYPE_NAMED_DATATYPE -> return . Some . Datatype $ h
      _ -> throw . H5Exception (-1) [] . Just $ "unknown object type"

openByIndex :: (MonadIO m, MonadMask m) => Object t -> Int -> m (Some Object)
openByIndex parent i = do
  n <- getSize parent
  when (i < 0 || i >= n) . error $ "invalid index: " <> show i
  r <- (h5Check msg =<<) . liftIO $ h5o_open_by_idx h "." H5_INDEX_NAME H5_ITER_INC i H5P_DEFAULT
  _constructObject r
  where
    !h = getRawHandle parent
    msg = Just $ "error opening object at index " <> show i

openByName :: (MonadIO m, MonadMask m) => Object t -> Text -> m (Some Object)
openByName parent path = do
  r <- (h5Check msg =<<) . liftIO $ h5o_open (getRawHandle parent) (toString path) H5P_DEFAULT
  _constructObject r
  where
    msg = Just $ "error opening object at path " <> show path

byIndex :: (MonadIO m, MonadMask m) => Group -> Int -> (Some Object -> m a) -> m a
byIndex g i = bracket (openByIndex g i) (\(Some x) -> close x)

byName :: (MonadIO m, MonadMask m) => Object t -> Text -> (Some Object -> m a) -> m a
byName object path = bracket (openByName object path) (\(Some x) -> close x)

withRoot :: (MonadIO m, MonadMask m) => File -> (Group -> m a) -> m a
withRoot file f = byName file "/" $ \case
  (Some group@(Group _)) -> f group
  _ -> error "unreachable"

-- h5Name :: (MonadIO m, MonadThrow m) => H5Handle -> m Text
-- h5Name handle =
--   let h = unHandle handle
--    in checkValidHandle handle >> do
--         liftIO (h5i_get_name h nullPtr 0) >>= \r -> case compare r 0 of
--           GT -> fmap toText . liftIO $
--             allocaBytes (r + 1) $ \s ->
--               h5i_get_name h s (r + 1) >>= h5Check msg >> peekCString s
--           EQ -> return ""
--           LT -> throwIO $ H5Exception (fromIntegral r) [] msg
--   where
--     msg = Just "error getting object's name"

-- h5OpenByIndex :: (MonadIO m, MonadThrow m) => H5Handle -> Int -> m H5Handle
-- h5OpenByIndex parent index =
--   checkValidHandle parent >> do
--     groupSize <- h5Size parent
--     when (index < 0 || index >= groupSize) . error $ "invalid index: " <> show index
--     r <- liftIO $ h5o_open_by_idx (unHandle parent) "." H5_INDEX_NAME H5_ITER_INC index H5P_DEFAULT
--     H5Group <$> h5Check (Just $ "error accessing object at index " <> show index) r

-- h5ByIndex :: (MonadIO m, MonadMask m) => H5Handle -> Int -> (H5Handle -> m b) -> m b
-- h5ByIndex parent index = undefined -- bracket (h5OpenByIndex parent index) closeHandle

foldM :: (MonadIO m, MonadMask m) => (a -> Some Object -> m a) -> a -> Object t -> m a
foldM !f !acc₀ = \case
  x@(File _) -> withRoot x $ \g -> go g 0 acc₀
  g@(Group _) -> go g 0 acc₀
  _ -> error "Oops!"
  where
    go !group !i !acc =
      getSize group >>= \n ->
        if i < n
          then byIndex group i (f acc) >>= go group (i + 1)
          else return acc

makeGroup :: (MonadIO m, MonadThrow m) => Object t -> Text -> m ()
makeGroup parent path = liftIO create >>= h5Check msg >>= \h -> close (Group h)
  where
    create = h5g_create (getRawHandle parent) (toString path) H5P_DEFAULT H5P_DEFAULT H5P_DEFAULT
    msg = Just $ "error creating group " <> show path

delete :: (MonadIO m, MonadThrow m) => Object t -> Text -> m ()
delete parent path = void . h5Check msg =<< liftIO delete
  where
    delete = h5l_delete (getRawHandle parent) (toString path) H5P_DEFAULT
    msg = Just $ "error deleting " <> show path

-- h5FoldlM :: (MonadIO m, MonadMask m) => (a -> H5Handle -> m a) -> a -> H5Handle -> m a
-- h5FoldlM f acc0 parent = go 0 acc0
--   where
--     go !i !acc =
--       h5Size parent >>= \n ->
--         if i < n
--           then h5ByIndex parent i (f acc) >>= go (i + 1)
--           else return acc

-- h5CreateGroup :: (MonadIO m, MonadThrow m) => H5Handle -> Text -> m H5Handle
-- h5CreateGroup parent path =
--   fmap H5Group $
--     checkValidHandle parent >> create parent path >>= h5Check msg
--   where
--     create h p =
--       liftIO $
--         h5g_create (unHandle h) (toString p) H5P_DEFAULT H5P_DEFAULT H5P_DEFAULT
--     msg = Just $ "error creating group " <> show path

-- h5Delete :: (MonadIO m, MonadThrow m) => H5Handle -> Text -> m ()
-- h5Delete parent path = void $ checkValidHandle parent >> delete parent path >>= h5Check msg
--   where
--     delete h p = liftIO $ h5l_delete (unHandle h) (toString p) H5P_DEFAULT
--     msg = Just $ "error deleting " <> show path

-- ** Datasets

-- data H5ConcreteBlob a = H5ConcreteBlob
--   { h5ConcreteBlobData :: {-# UNPACK #-} !(Vector a),
--     h5ConcreteBlobDims :: ![Int]
--   }
--   deriving stock (Show)

data Blob a where
  Blob :: KnownDatatype a => {-# UNPACK #-} !(Vector a) -> ![Int] -> Blob a

deriving stock instance Show a => Show (Blob a)

-- data H5Blob = forall a. H5KnownDatatype a => H5Blob !(H5ConcreteBlob a)

-- newtype H5Dataset = H5Dataset {unDataset :: Hid}
--
-- data H5DatasetInfo = H5DatasetInfo ![Int] !H5T_class_t !Int
--   deriving stock (Eq, Show)

-- getDims :: H5Blob -> [Int]
-- getDims (H5Blob x) = h5ConcreteBlobDims x

-- ** Objects

data ObjectType = FileTy | GroupTy | DatasetTy | DatatypeTy

data Object (k :: ObjectType) where
  File :: Hid -> Object 'FileTy
  Group :: Hid -> Object 'GroupTy
  Dataset :: Hid -> Object 'DatasetTy
  Datatype :: Hid -> Object 'DatatypeTy

type File = Object 'FileTy

type Group = Object 'GroupTy

type Dataset = Object 'DatasetTy

type Datatype = Object 'DatatypeTy

getRawHandle :: Object t -> Hid
getRawHandle (File h) = h
getRawHandle (Group h) = h
getRawHandle (Dataset h) = h
getRawHandle (Datatype h) = h

_getObjectType :: (MonadIO m, MonadThrow m) => Hid -> m H5O_type_t
_getObjectType h =
  liftIO (h5o_get_type h) >>= \case
    Left code -> h5Fail (Just "error getting group size") code
    Right t -> return t

closeObjectImpl :: (MonadIO m, MonadThrow m) => Hid -> m ()
closeObjectImpl = void . h5Check (Just "error closing object") <=< liftIO . h5o_close

closeFileImpl :: (MonadIO m, MonadThrow m) => Hid -> m ()
closeFileImpl = void . h5Check (Just "error closing file") <=< liftIO . h5f_close

close :: (MonadIO m, MonadThrow m) => Object t -> m ()
close = \case
  (File h) -> closeFileImpl $ h
  object -> closeObjectImpl . getRawHandle $ object

-- checkValidHandle parent >> do
--   groupSize <- h5Size parent
--   when (index < 0 || index >= groupSize) . error $ "invalid index: " <> show index
--   r <- liftIO $ h5o_open_by_idx (unHandle parent) "." H5_INDEX_NAME H5_ITER_INC index H5P_DEFAULT
--   H5Group <$> h5Check (Just $ "error accessing object at index " <> show index) r

-- ** Datatypes

-- newtype H5Datatype = H5Datatype {unDatatype :: Hid}

-- eqDatatypeImpl :: H5Datatype -> H5Datatype -> Bool
-- eqDatatypeImpl !(H5Datatype h₁) !(H5Datatype h₂) =
--   unsafePerformIO $
--     fmap (> 0) $ h5Check msg =<< h5t_equal h₁ h₂
--   where
--     msg = Just $ "h5t_equal failed"
-- {-# NOINLINE eqDatatypeImpl #-}

-- showDatatypeImpl :: H5Datatype -> String
-- showDatatypeImpl !(H5Datatype h) = unsafePerformIO $ do
--   alloca $ \sizePtr -> do
--     check =<< h5lt_dtype_to_text h nullPtr H5LT_DDL sizePtr
--     size <- peek sizePtr
--     allocaArray (fromIntegral size) $ \sPtr -> do
--       check =<< h5lt_dtype_to_text h sPtr H5LT_DDL sizePtr
--       peekCString sPtr
--   where
--     check c =
--       when (c < 0) . throw . H5Exception (fromIntegral c) [] . Just $
--         "failed to obtain textual representation of the datatype"
-- {-# NOINLINE showDatatypeImpl #-}

-- instance Eq H5Datatype where (==) = eqDatatypeImpl

-- instance Show H5Datatype where show = showDatatypeImpl

eqDatatype :: Datatype -> Datatype -> Bool
eqDatatype !(Datatype h₁) !(Datatype h₂) =
  unsafePerformIO
    $! fmap (> 0) . h5Check (Just "failed to compare datatypes") =<< h5t_equal h₁ h₂
{-# NOINLINE eqDatatype #-}

showDatatype :: Datatype -> String
showDatatype !(Datatype h) =
  unsafePerformIO
    $! alloca
    $ \sizePtr -> do
      void . h5Check msg =<< h5lt_dtype_to_text h nullPtr H5LT_DDL sizePtr
      size <- peek sizePtr
      allocaArray (fromIntegral size) $ \sPtr -> do
        h5lt_dtype_to_text h sPtr H5LT_DDL sizePtr >>= void . h5Check msg >> peekCString sPtr
  where
    msg = Just $ "failed to obtain textual representation of the datatype"
{-# NOINLINE showDatatype #-}

instance Eq Datatype where (==) = eqDatatype

instance Show Datatype where show = showDatatype

class (Typeable a, Storable a) => NativeDatatype a where
  nativeType :: proxy a -> Datatype

-- class (Typeable a, Storable a) => H5KnownDatatype a where
--   withDatatype :: (MonadIO m, MonadThrow m) => proxy a -> (H5Datatype -> m b) -> m b

class (Typeable a, Storable a) => KnownDatatype a where
  withDatatype :: (MonadIO m, MonadThrow m) => proxy a -> (Datatype -> m b) -> m b

instance NativeDatatype Float where
  nativeType _ = Datatype $ unsafePerformIO $! peek h5t_NATIVE_FLOAT

instance NativeDatatype Double where
  nativeType _ = Datatype $ unsafePerformIO $! peek h5t_NATIVE_DOUBLE

instance KnownDatatype Float where withDatatype p f = f (nativeType p)

instance KnownDatatype Double where withDatatype p f = f (nativeType p)

-- toConcreteBlob :: Typeable a => H5Blob -> Maybe (H5ConcreteBlob a)
-- toConcreteBlob (H5Blob x) = cast x
-- {-# INLINE toConcreteBlob #-}

-- pattern H5Int16Blob :: Vector Int16 -> [Int] -> H5Blob
-- pattern H5Int16Blob v n <- (toConcreteBlob @Int16 -> Just (H5ConcreteBlob v n))

-- pattern H5Int32Blob :: Vector Int32 -> [Int] -> H5Blob
-- pattern H5Int32Blob v n <- (toConcreteBlob @Int32 -> Just (H5ConcreteBlob v n))

-- pattern H5Int64Blob :: Vector Int64 -> [Int] -> H5Blob
-- pattern H5Int64Blob v n <- (toConcreteBlob @Int64 -> Just (H5ConcreteBlob v n))

-- pattern H5FloatBlob :: Vector Float -> [Int] -> H5Blob
-- pattern H5FloatBlob v n <- (toConcreteBlob @Float -> Just (H5ConcreteBlob v n))

-- pattern H5DoubleBlob :: Vector Double -> [Int] -> H5Blob
-- pattern H5DoubleBlob v n <- (toConcreteBlob @Double -> Just (H5ConcreteBlob v n))

-- class (Typeable a, Storable a) => H5DataType a where
--   h5GetClass :: proxy a -> H5T_class_t

-- instance H5DataType Double where h5GetClass _ = H5T_FLOAT

-- instance H5DataType Float where h5GetClass _ = H5T_FLOAT

-- instance H5DataType Int16 where h5GetClass _ = H5T_INTEGER

-- instance H5DataType Int32 where h5GetClass _ = H5T_INTEGER

-- instance H5DataType Int64 where h5GetClass _ = H5T_INTEGER

-- prettyName :: H5T_class_t -> Int -> Text
-- prettyName c n = case c of
--   H5T_FLOAT -> case n of
--     4 -> "Float"
--     8 -> "Double"
--     _ -> defaultName
--   _ -> defaultName
--   where
--     defaultName = show n <> " byte " <> show c

-- checkMatch :: forall a m proxy. (H5DataType a, MonadThrow m) => proxy a -> H5T_class_t -> Int -> m ()
-- checkMatch _ typeClass typeSize = do
--   unless (typeClass == h5GetClass proxy && typeSize == sizeOf' proxy) . throw . H5Exception (-1) . Just $
--     "type mismatch: " <> prettyName typeClass typeSize <> "; expected " <> show (typeOf' proxy)
--   where
--     proxy = Proxy @a

-- checkValidHandle :: (MonadIO m, MonadThrow m) => H5Handle -> m ()
-- checkValidHandle handle
--   | h < 0 = error $ "handle is invalid: " <> show handle
--   | otherwise =
--     (liftIO $ h5i_is_valid h) >>= \status -> case compare status 0 of
--       LT -> throwIO $ H5Exception (fromIntegral status) [] Nothing
--       EQ -> throwIO . H5Exception (-1) [] . Just $ "handle is invalid: " <> show handle
--       _ -> return ()
--   where
--     h = unHandle handle

-- h5FindDataset :: (MonadIO m, MonadThrow m) => H5Handle -> Text -> m Bool
-- h5FindDataset group name =
--   fmap (toEnum . fromIntegral) $
--     checkValidHandle group >> find group name >>= h5Check msg
--   where
--     find h p = liftIO $ h5lt_find_dataset (unHandle h) (toString p)
--     msg = Just $ "error searching for dataset " <> show name

-- h5GetNumDims :: (MonadIO m, MonadThrow m) => H5Handle -> Text -> m Int
-- h5GetNumDims parent path = do
--   checkValidHandle parent
--   fmap fromIntegral . liftIO . alloca $ \rankPtr ->
--     getNumDims rankPtr >>= h5Check msg >> peek rankPtr
--   where
--     getNumDims ptr = h5lt_get_dataset_ndims (unHandle parent) (toString path) ptr
--     msg = Just $ "failed to get number of dimensions of " <> show path

_getNumDims :: (MonadIO m, MonadThrow m) => Object t -> Text -> m Int
_getNumDims parent path = fmap fromIntegral . liftIO . alloca $ \rankPtr ->
  getNumDims rankPtr >>= h5Check msg >> peek rankPtr
  where
    getNumDims = h5lt_get_dataset_ndims (getRawHandle parent) (toString path)
    msg = Just $ "failed to get number of dimensions of " <> show path

getDims :: (MonadIO m, MonadThrow m) => Object t -> Text -> m [Int]
getDims parent path = do
  numDims <- _getNumDims parent path
  r <- liftIO $
    allocaArray numDims $ \dimsPtr ->
      alloca $ \classIdPtr ->
        alloca $ \typeSizePtr -> do
          status <- getInfo dimsPtr classIdPtr typeSizePtr
          if status < 0
            then return . Left $ status
            else Right <$> fmap fromIntegral <$> peekArray numDims dimsPtr
  case r of
    Left code -> h5Fail (Just "error getting dimensions") code
    Right dims -> return dims
  where
    getInfo = h5lt_get_dataset_info (getRawHandle parent) (toString path)

-- h5GetDatasetInfo :: (MonadIO m, MonadThrow m) => H5Handle -> Text -> m H5DatasetInfo
-- h5GetDatasetInfo parent path = do
--   numDims <- h5GetNumDims parent path
--   liftIO $
--     allocaArray numDims $ \dimsPtr ->
--       alloca $ \classIdPtr ->
--         alloca $ \typeSizePtr -> do
--           void . h5Check msg =<< getInfo dimsPtr classIdPtr typeSizePtr
--           H5DatasetInfo
--             <$> (fmap fromIntegral <$> peekArray numDims dimsPtr)
--             <*> (toEnum . fromIntegral <$> peek classIdPtr)
--             <*> (fromIntegral <$> peek typeSizePtr)
--   where
--     getInfo = h5lt_get_dataset_info (unHandle parent) (toString path)
--     msg = Nothing

-- unsafeReadDataset ::
--   forall b m a.
--   (MonadIO m, MonadThrow m, Storable a, Storable b, Coercible a b) =>
--   (Hid -> String -> Ptr a -> IO Herr) ->
--   H5Handle ->
--   Text ->
--   [Int] ->
--   m (H5ConcreteBlob b)
-- unsafeReadDataset ffiRead parent path dims = do
--   v <- liftIO $ MV.unsafeNew (product dims)
--   void . h5Check msg =<< liftIO (MV.unsafeWith v read)
--   buffer <- V.unsafeCast <$> liftIO (V.unsafeFreeze v)
--   return $ H5ConcreteBlob buffer dims
--   where
--     read = ffiRead (unHandle parent) (toString path)
--     msg = Just $ "error reading dataset " <> show path

-- readTypedDataset ::
--   forall a m proxy.
--   (H5KnownDatatype a, MonadIO m, MonadMask m) =>
--   proxy a ->
--   H5Handle ->
--   Text ->
--   m (H5ConcreteBlob a)
-- readTypedDataset p parent path = do
--   (H5DatasetInfo dims _ _) <- h5GetDatasetInfo parent path
--   bracket (getDatatype parent path) closeDatatype $ \dtype ->
--     withDatatype p $ \dtype' -> do
--       when (dtype /= dtype') . throw . H5Exception (-1) [] . Just
--         $! "dataset has wrong datatype: " <> show dtype <> "; expected " <> show dtype'
--       _unsafeReadDataset p parent path dims

readDataset :: (MonadIO m, MonadMask m) => Dataset -> m (Some Blob)
readDataset dataset = do
  _withDatatype dataset $ \dtype ->
    guessDatatype dtype $ \p -> Some <$> readDataset' p dataset

-- _unsafeReadDataset ::
--   forall a m proxy.
--   (H5KnownDatatype a, MonadIO m, MonadMask m) =>
--   proxy a ->
--   H5Handle ->
--   Text ->
--   [Int] ->
--   m (H5ConcreteBlob a)
-- _unsafeReadDataset p parent path dims = do
--   withDatatype p $ \dtype -> do
--     let read = h5lt_read_dataset (unHandle parent) (toString path) (unDatatype dtype) . castPtr
--         msg = Just $ "failed to read dataset " <> show path
--     v <- liftIO $ MV.unsafeNew (product dims)
--     void . h5Check msg =<< liftIO (MV.unsafeWith v read)
--     v' <- liftIO $ V.unsafeFreeze v
--     return $ H5ConcreteBlob v' dims

readDataset' :: forall a m proxy. (KnownDatatype a, MonadIO m, MonadMask m) => proxy a -> Dataset -> m (Blob a)
readDataset' proxy dataset =
  getDims dataset "." >>= \dims ->
    withDatatype proxy $ \dtype -> do
      checkDatatypes dtype
      let read = h5lt_read_dataset (getRawHandle dataset) "." (getRawHandle dtype) . castPtr
      v <- liftIO $ MV.unsafeNew (product dims)
      void . h5Check (Just "failed to read dataset") =<< liftIO (MV.unsafeWith v read)
      v' <- liftIO $ V.unsafeFreeze v
      return $ Blob v' dims
  where
    checkDatatypes dtype =
      _withDatatype dataset $ \dtype' ->
        when (dtype /= dtype') . throw . H5Exception (-1) [] . Just
          $! "dataset has wrong datatype: " <> show dtype' <> "; expected " <> show dtype

guessDatatype ::
  (MonadIO m, MonadThrow m) =>
  Datatype ->
  (forall a proxy. KnownDatatype a => proxy a -> m b) ->
  m b
guessDatatype dtype f
  | dtype == nativeType (Proxy @Float) = f (Proxy @Float)
  | dtype == nativeType (Proxy @Double) = f (Proxy @Double)
  | otherwise = undefined

makeDataset ::
  forall a m t.
  (MonadIO m, MonadThrow m, KnownDatatype a) =>
  Object t ->
  Text ->
  Blob a ->
  m ()
makeDataset parent path (Blob v dims)
  | product dims == V.length v = do
    (void . h5Check msg =<<) $
      liftIO $
        withDatatype (Proxy @a) $ \dtype ->
          withArrayLen (fromIntegral <$> dims) $ \rank dimsPtr ->
            V.unsafeWith v $ \buffer ->
              h5lt_make_dataset (getRawHandle parent) (toString path) rank dimsPtr (getRawHandle dtype) (castPtr buffer)
  | otherwise = error $ "product of dimensions " <> show dims <> " does not match the size of vector " <> show (V.length v)
  where
    msg = Just $ "failed to create dataset " <> show path

-- openDataset :: (MonadIO m, MonadThrow m) => H5Handle -> Text -> m H5Dataset
-- openDataset parent path =
--   fmap H5Dataset . (h5Check msg =<<) . liftIO $
--     h5d_open (unHandle parent) (toString path) H5P_DEFAULT
--   where
--     msg = Just $ "failed to open dataset " <> show path

-- closeDataset :: (MonadIO m, MonadThrow m) => H5Dataset -> m ()
-- closeDataset = void . h5Check (Just "could not close dataset") <=< liftIO . h5d_close . unDataset

-- withDataset :: (MonadIO m, MonadMask m) => H5Handle -> Text -> (H5Dataset -> m a) -> m a
-- withDataset parent path = bracket (openDataset parent path) closeDataset

-- closeDatatype :: (MonadIO m, MonadThrow m) => H5Datatype -> m ()
-- closeDatatype = void . h5Check (Just "could not close datatype") <=< liftIO . h5t_close . unDatatype

-- getDatatype :: (MonadIO m, MonadMask m) => H5Handle -> Text -> m H5Datatype
-- getDatatype parent path =
--   fmap H5Datatype . withDataset parent path $
--     liftIO . h5d_get_type . unDataset >=> h5Check (Just "failed to get datatype of a dataset")

_withDatatype :: (MonadIO m, MonadMask m) => Dataset -> (Datatype -> m a) -> m a
_withDatatype object = bracket (Datatype <$> acquire object) close
  where
    acquire = liftIO . h5d_get_type . getRawHandle >=> h5Check (Just "failed to get datatype of a dataset")
