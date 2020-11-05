{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Copyright: (c) 2020 Tom Westerhout
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
  ( -- * Quickstart

    -- | It is suggested to use qualified imports with @hdf5-hs@ package
    -- @
    --     import qualified Data.HDF5 as H5
    -- @

    -- | Hello world!
    withFile,
    withRoot,
    IOMode (..),
    getName,
    getSize,
    getDims,
    byIndex,
    byName,
    foldM,
    Object (..),
    Blob (..),
    Attr (..),
    Some (..),
    makeGroup,
    delete,
    readDataset,
    readDataset',
    makeDataset,
    readAttribute',
    disableDiagOutput,
    H5Exception (..),
    -- toConcreteBlob,
    -- pattern H5Int16Blob,
    -- pattern H5Int32Blob,
    -- pattern H5Int64Blob,
    -- pattern H5FloatBlob,
    -- pattern H5DoubleBlob,
  )
where

import Control.Exception.Safe hiding (handle)
import Data.HDF5.Internal
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
import qualified GHC.TypeLits
import Relude hiding (find, group, withFile)
import System.Directory (doesFileExist)
import System.IO (IOMode (..))
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------
-- Error Handling
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Files
--------------------------------------------------------------------------------

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

withFile :: (MonadIO m, MonadMask m) => FilePath -> IOMode -> (File -> m a) -> m a
withFile path mode = bracket (openFile path mode) close

withRoot :: (MonadIO m, MonadMask m, FileOrGroup t) => Object t -> (Group -> m a) -> m a
withRoot file f = byName file "/" $ \case
  (Some group@(Group _)) -> f group
  _ -> error "unreachable"

--------------------------------------------------------------------------------
-- Objects
--------------------------------------------------------------------------------

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

type family FileOrGroup (t :: ObjectType) :: Constraint where
  FileOrGroup FileTy = ()
  FileOrGroup GroupTy = ()
  FileOrGroup t =
    GHC.TypeLits.TypeError
      ( GHC.TypeLits.Text "Expected either a File or a Group, but got "
          GHC.TypeLits.:<>: GHC.TypeLits.ShowType t
      )

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

delete :: (MonadIO m, MonadThrow m) => Object t -> Text -> m ()
delete parent path = void . h5Check msg =<< liftIO delete
  where
    delete = h5l_delete (getRawHandle parent) (toString path) H5P_DEFAULT
    msg = Just $ "error deleting " <> show path

--------------------------------------------------------------------------------
-- Groups
--------------------------------------------------------------------------------

makeGroup :: (MonadIO m, MonadThrow m) => Object t -> Text -> m ()
makeGroup parent path = liftIO create >>= h5Check msg >>= \h -> close (Group h)
  where
    create = h5g_create (getRawHandle parent) (toString path) H5P_DEFAULT H5P_DEFAULT H5P_DEFAULT
    msg = Just $ "error creating group " <> show path

--------------------------------------------------------------------------------
-- Datasets
--------------------------------------------------------------------------------

data Blob a where
  Blob :: KnownDatatype a => ![Int] -> {-# UNPACK #-} !(Vector a) -> Blob a

deriving stock instance Show a => Show (Blob a)

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

readDataset :: (MonadIO m, MonadMask m) => Dataset -> m (Some Blob)
readDataset dataset = do
  _withDatatype dataset $ \dtype ->
    guessDatatype dtype $ \p -> Some <$> readDataset' p dataset

readDataset' :: (KnownDatatype a, MonadIO m, MonadMask m) => proxy a -> Dataset -> m (Blob a)
readDataset' proxy dataset =
  getDims dataset "." >>= \dims ->
    withDatatype proxy $ \dtype -> do
      checkDatatypes dtype
      let read = h5lt_read_dataset (getRawHandle dataset) "." (getRawHandle dtype) . castPtr
      v <- liftIO $ MV.unsafeNew (product dims)
      void . h5Check (Just "failed to read dataset") =<< liftIO (MV.unsafeWith v read)
      v' <- liftIO $ V.unsafeFreeze v
      return $ Blob dims v'
  where
    checkDatatypes dtype =
      _withDatatype dataset $ \dtype' ->
        when (dtype /= dtype') . throw . H5Exception (-1) [] . Just
          $! "dataset has wrong datatype: " <> show dtype' <> "; expected " <> show dtype

makeDataset ::
  forall a m t.
  (MonadIO m, MonadThrow m, KnownDatatype a) =>
  Object t ->
  Text ->
  Blob a ->
  m ()
makeDataset parent path (Blob dims v )
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

_withDatatype :: (MonadIO m, MonadMask m) => Dataset -> (Datatype -> m a) -> m a
_withDatatype object = bracket (Datatype <$> acquire object) close
  where
    acquire = liftIO . h5d_get_type . getRawHandle >=> h5Check (Just "failed to get datatype of a dataset")

--------------------------------------------------------------------------------
-- Datatypes
--------------------------------------------------------------------------------

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

class (Typeable a, Storable a) => KnownDatatype a where
  withDatatype :: (MonadIO m, MonadThrow m) => proxy a -> (Datatype -> m b) -> m b

instance NativeDatatype Float where
  nativeType _ = Datatype $ unsafePerformIO $! peek h5t_NATIVE_FLOAT

instance NativeDatatype Double where
  nativeType _ = Datatype $ unsafePerformIO $! peek h5t_NATIVE_DOUBLE

instance KnownDatatype Float where withDatatype p f = f (nativeType p)

instance KnownDatatype Double where withDatatype p f = f (nativeType p)

guessDatatype ::
  (MonadIO m, MonadThrow m) =>
  Datatype ->
  (forall a proxy. KnownDatatype a => proxy a -> m b) ->
  m b
guessDatatype dtype f
  | dtype == nativeType (Proxy @Float) = f (Proxy @Float)
  | dtype == nativeType (Proxy @Double) = f (Proxy @Double)
  | otherwise = undefined

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------

data Attr a where
  Attr :: KnownDatatype a => a -> Attr a

deriving stock instance Show a => Show (Attr a)

newtype Attribute = Attribute {unAttribute :: Hid}

openAttribute :: (MonadIO m, MonadThrow m) => Object t -> Text -> m Attribute
openAttribute object name =
  fmap Attribute . (h5Check msg =<<) . liftIO $
    h5a_open (getRawHandle object) (toString name) H5P_DEFAULT
  where
    msg = Just $ "failed to open attribute " <> show name

closeAttribute :: (MonadIO m, MonadThrow m) => Attribute -> m ()
closeAttribute = void . h5Check (Just "error closing attribute") <=< liftIO . h5a_close . unAttribute

withAttribute :: (MonadIO m, MonadMask m) => Object t -> Text -> (Attribute -> m a) -> m a
withAttribute object name = bracket (openAttribute object name) closeAttribute

_withAttributeDatatype :: (MonadIO m, MonadMask m) => Attribute -> (Datatype -> m a) -> m a
_withAttributeDatatype attr = bracket (acquire attr) close
  where
    acquire = fmap Datatype . h5Check msg <=< liftIO . h5a_get_type . unAttribute
    msg = Just "failed to get attribute datatype"

readAttribute' :: forall a m t proxy. (KnownDatatype a, MonadIO m, MonadMask m) => proxy a -> Object t -> Text -> m (Attr a)
readAttribute' proxy object name =
  withAttribute object name $ \attr ->
    withDatatype proxy $ \dtype -> do
      checkDatatypes attr dtype
      let read = h5lt_get_attribute (getRawHandle object) "." (toString name) (getRawHandle dtype) . castPtr
      r <- liftIO $
        alloca $ \ptr ->
          read ptr >>= \c ->
            if c < 0 then return $ Left c else Right <$> peek ptr
      case r of
        Left c -> h5Fail msg c
        Right x -> return $ Attr x
  where
    msg = Just $ "failed to read attribute " <> show name
    checkDatatypes attr dtype =
      _withAttributeDatatype attr $ \dtype' ->
        when (dtype /= dtype') . throw . H5Exception (-1) [] . Just
          $! "attribute has wrong datatype: " <> show dtype' <> "; expected " <> show dtype

-- readAttribute' :: forall a m proxy. (KnownDatatype a, MonadIO m, MonadMask m) => proxy a -> Object t -> m (Blob a)
-- readAttribute' proxy object =
--   getDims dataset "." >>= \dims ->
--     withDatatype proxy $ \dtype -> do
--       checkDatatypes dtype
--       let read = h5lt_read_dataset (getRawHandle dataset) "." (getRawHandle dtype) . castPtr
--       v <- liftIO $ MV.unsafeNew (product dims)
--       void . h5Check (Just "failed to read dataset") =<< liftIO (MV.unsafeWith v read)
--       v' <- liftIO $ V.unsafeFreeze v
--       return $ Blob v' dims
--   where
--     checkDatatypes dtype =
--       _withDatatype dataset $ \dtype' ->
--         when (dtype /= dtype') . throw . H5Exception (-1) [] . Just
--           $! "dataset has wrong datatype: " <> show dtype' <> "; expected " <> show dtype
