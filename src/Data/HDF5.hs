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
  ( -- * Working with files

    -- | It is suggested to use qualified imports with @hdf5-hs@ package
    --
    -- > import qualified Data.HDF5 as H5
    --
    -- after which to open a hdf5 file for reading/writing you can use the
    -- 'withFile' bracket:
    --
    -- > withFile "myImportantData.h5" ReadMode $ \file ->
    -- >     ...
    --
    -- Note that 'ReadMode' comes from "System.IO" module. We use it here
    -- instead of HDF5-specific modes to stress that HDF5 files are no different
    -- from "normal" files.
    withFile,

    -- * Groups

    -- |
    makeGroup,

    -- * Datasets

    -- |
    Blob (..),
    readDataset,
    readDataset',
    makeDataset,
    getDims,

    -- * Attributes

    -- |
    hasAttribute,
    readAttribute,
    writeAttribute,
    deleteAttribute,

    -- * Objects

    -- |
    Object (..),
    -- File,
    -- Group,
    -- Dataset,
    -- Datatype,
    FileOrGroup,
    delete,
    getName,
    getSize,
    byIndex,
    byName,
    foldM,
    disableDiagOutput,
    H5Exception (..),

    -- * Re-exports

    -- | He-he-he
    IOMode (..),
    Some (..),
  )
where

import Control.Exception.Safe hiding (handle)
import Data.Complex
import Data.Constraint (Dict (..))
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
import Unsafe.Coerce

--------------------------------------------------------------------------------
-- Files
--------------------------------------------------------------------------------

openFile :: (MonadIO m, MonadThrow m) => Text -> IOMode -> m File
openFile path mode =
  liftIO $
    doesFileExist (toString path) >>= \case
      True -> case mode of
        ReadMode -> h5f_open path H5F_ACC_RDONLY H5P_DEFAULT
        WriteMode -> h5f_create path H5F_ACC_TRUNC H5P_DEFAULT H5P_DEFAULT
        _ -> h5f_open path H5F_ACC_RDWR H5P_DEFAULT
      False -> case mode of
        ReadMode -> fileNotFound
        _ -> h5f_create path H5F_ACC_EXCL H5P_DEFAULT H5P_DEFAULT
  where
    fileNotFound = throw . H5Exception (-1) [] . Just $ "file " <> show path <> " not found"

-- | Do some work with a file.
withFile ::
  (MonadIO m, MonadMask m) =>
  -- | filename
  Text ->
  -- | mode in which to open the file
  IOMode ->
  -- | action to perform
  (File -> m a) ->
  m a
withFile path mode = bracket (openFile path mode) close

--------------------------------------------------------------------------------
-- Objects
--------------------------------------------------------------------------------

-- | A 'Constraint' to specify that the operation works for 'File' and 'Group',
-- but not for other 'Object' types like 'Dataset' and 'Datatype'.
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

_getObjectType :: MonadIO m => Hid -> m H5O_type_t
_getObjectType = liftIO . h5o_get_type

close :: (MonadIO m) => Object t -> m ()
close = \case
  (File h) -> liftIO . h5f_close $ h
  object -> liftIO . h5o_close . getRawHandle $ object

getSize :: (MonadIO m, MonadThrow m) => Object t -> m Int
getSize = h5Check (Just "error getting group size") <=< liftIO . h5g_get_num_objs . getRawHandle

getName :: (MonadIO m, MonadThrow m) => Object t -> m Text
getName object =
  liftIO $
    h5i_get_name h nullPtr 0 >>= \r ->
      if r > 0
        then fmap toText $
          allocaBytes (fromIntegral r + 1) $ \s ->
            h5i_get_name h s (fromIntegral r + 1) >> peekCString s
        else return ""
  where
    !h = getRawHandle object

_constructObject :: (MonadIO m, MonadMask m) => Hid -> m (Some Object)
_constructObject h =
  flip onException (liftIO $ h5o_close h) $
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
  r <- (h5Check msg =<<) . liftIO $ h5o_open (getRawHandle parent) path H5P_DEFAULT
  _constructObject r
  where
    msg = Just $ "error opening object at path " <> show path

byIndex :: (MonadIO m, MonadMask m, FileOrGroup t) => Object t -> Int -> (Some Object -> m a) -> m a
byIndex g i = bracket (openByIndex g i) (\(Some x) -> close x)

byName :: (MonadIO m, MonadMask m) => Object t -> Text -> (Some Object -> m a) -> m a
byName object path = bracket (openByName object path) (\(Some x) -> close x)

foldM :: forall a t m. (MonadIO m, MonadMask m, FileOrGroup t) => (a -> Some Object -> m a) -> a -> Object t -> m a
foldM !f !acc₀ = \g -> go g 0 acc₀
  where
    -- \case
    --   g@(File _) -> withRoot x $ \g -> go g 0 acc₀
    --   g@(Group _) -> go g 0 acc₀
    --   _ -> error "Oops!"

    go :: Object t -> Int -> a -> m a
    go !group !i !acc =
      getSize group >>= \n ->
        if i < n
          then byIndex group i (f acc) >>= go group (i + 1)
          else return acc

delete :: MonadIO m => Object t -> Text -> m ()
delete parent path = liftIO $ h5l_delete (getRawHandle parent) path H5P_DEFAULT

--------------------------------------------------------------------------------
-- Groups
--------------------------------------------------------------------------------

makeGroup :: (MonadIO m, MonadThrow m) => Object t -> Text -> m ()
makeGroup parent path = create >>= close
  where
    create = liftIO $ h5g_create (getRawHandle parent) path H5P_DEFAULT H5P_DEFAULT H5P_DEFAULT

--------------------------------------------------------------------------------
-- Datasets
--------------------------------------------------------------------------------

data Blob a where
  Blob :: (Storable a, KnownDatatype a) => ![Int] -> {-# UNPACK #-} !(Vector a) -> Blob a

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
readDataset dataset = undefined

-- readDataset :: (MonadIO m, MonadMask m) => Dataset -> m (Some Blob)
-- readDataset dataset = do
--   _withDatatype dataset $ \dtype ->
--     guessDatatype dtype $ \p -> Some <$> readDataset' p dataset

readDataset' :: (Storable a, KnownDatatype a, MonadIO m, MonadMask m) => proxy a -> Dataset -> m (Blob a)
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
  (MonadIO m, MonadThrow m, Storable a, KnownDatatype a) =>
  Object t ->
  Text ->
  Blob a ->
  m ()
makeDataset parent path (Blob dims v)
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
_withDatatype object = bracket (liftIO . h5d_get_type . getRawHandle $ object) close

--------------------------------------------------------------------------------
-- Datatypes
--------------------------------------------------------------------------------

eqDatatype :: Datatype -> Datatype -> Bool
eqDatatype (Datatype h₁) (Datatype h₂) = unsafePerformIO $ h5t_equal h₁ h₂
{-# NOINLINE eqDatatype #-}

showDatatype :: Datatype -> String
showDatatype !(Datatype h) =
  unsafePerformIO
    $! alloca
    $ \sizePtr -> do
      size <- h5lt_dtype_to_text h nullPtr H5LT_DDL sizePtr >>= h5Check msg >> peek sizePtr
      allocaArray (fromIntegral size) $ \sPtr -> do
        h5lt_dtype_to_text h sPtr H5LT_DDL sizePtr >>= void . h5Check msg >> peekCString sPtr
  where
    msg = Just $ "failed to obtain textual representation of the datatype"
{-# NOINLINE showDatatype #-}

instance Eq Datatype where (==) = eqDatatype

instance Show Datatype where show = showDatatype

withComplexDatatype ::
  forall a m b proxy.
  (MonadIO m, MonadMask m, Storable a, KnownDatatype' a) =>
  proxy (Complex a) ->
  (Datatype -> m b) ->
  m b
withComplexDatatype _ = bracket acquire close
  where
    acquire = withDatatype' (Proxy @a) $ \dtype -> do
      object <- liftIO $ h5t_create H5T_COMPOUND (2 * size)
      flip onException (close object) $
        liftIO $ do
          h5t_insert (getRawHandle object) "r" 0 (getRawHandle dtype)
          h5t_insert (getRawHandle object) "i" size (getRawHandle dtype)
      return object
    size = sizeOf (undefined :: a)

-- | Specifies size of a 'Datatype'.
--
-- We want to support text attributes which usually have variable size. Hence,
-- just adding 'Storable' constraint is not a solution.
data DatatypeSize
  = FixedSize {-# UNPACK #-} !Int
  | VariableSize

class KnownDatatype' a where
  withDatatype' :: (MonadIO m, MonadMask m) => proxy a -> (Datatype -> m b) -> m b

  hasStorable :: proxy a -> Maybe (Dict (Storable a))
  hasStorable _ = Nothing

  h5Peek :: Datatype -> Ptr () -> IO a
  default h5Peek :: Storable a => Datatype -> Ptr () -> IO a
  h5Peek _ = peek . castPtr
  h5Poke :: Datatype -> Ptr () -> a -> IO ()
  default h5Poke :: Storable a => Datatype -> Ptr () -> a -> IO ()
  h5Poke _ p = poke (castPtr p)

instance KnownDatatype' Int where
  withDatatype' _ = fromStaticPointer h5t_NATIVE_INT64
  hasStorable _ = Just Dict

instance KnownDatatype' Float where
  withDatatype' _ = fromStaticPointer h5t_NATIVE_FLOAT
  hasStorable _ = Just Dict

instance KnownDatatype' Double where
  withDatatype' _ = fromStaticPointer h5t_NATIVE_DOUBLE
  hasStorable _ = Just Dict

instance KnownDatatype' (Complex Float) where
  withDatatype' = withComplexDatatype
  hasStorable _ = Just Dict

instance KnownDatatype' (Complex Double) where
  withDatatype' = withComplexDatatype
  hasStorable _ = Just Dict

class KnownDatatype a where
  withDatatype :: (MonadIO m, MonadThrow m) => proxy a -> (Datatype -> m b) -> m b

fromStaticPointer :: MonadIO m => Ptr Hid -> (Datatype -> m b) -> m b
fromStaticPointer p f = f . Datatype =<< liftIO (peek p)

-- instance KnownDatatype Float where withDatatype p f = f (nativeType p)

-- instance KnownDatatype Double where withDatatype p f = f (nativeType p)

-- guessDatatype ::
--   (MonadIO m, MonadThrow m) =>
--   Datatype ->
--   (forall a proxy. KnownDatatype a => proxy a -> m b) ->
--   m b
-- guessDatatype dtype f
--   | dtype == nativeType (Proxy @Float) = f (Proxy @Float)
--   | dtype == nativeType (Proxy @Double) = f (Proxy @Double)
--   | otherwise = undefined

--------------------------------------------------------------------------------
-- Dataspaces
--------------------------------------------------------------------------------

newtype Dataspace = Dataspace {unDataspace :: Hid}

withDataspace :: (MonadIO m, MonadMask m) => H5S_class_t -> (Dataspace -> m a) -> m a
withDataspace c =
  bracket (liftIO $ Dataspace <$> h5s_create c) (liftIO . h5s_close . unDataspace)

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------

newtype Attribute = Attribute {unAttribute :: Hid}

openAttribute :: MonadIO m => Object t -> Text -> m Attribute
openAttribute object name = liftIO $ Attribute <$> h5a_open (getRawHandle object) name H5P_DEFAULT

createAttribute :: (MonadIO m, MonadMask m) => Object t -> Text -> Datatype -> m Attribute
createAttribute object name dtype =
  withDataspace H5S_SCALAR $ \dataspace ->
    liftIO $
      Attribute
        <$> h5a_create2
          (getRawHandle object)
          name
          (getRawHandle dtype)
          (unDataspace dataspace)
          H5P_DEFAULT
          H5P_DEFAULT

withAttribute :: (MonadIO m, MonadMask m) => Object t -> Text -> (Attribute -> m a) -> m a
withAttribute object name = bracket (openAttribute object name) (liftIO . h5a_close . unAttribute)

withAttributeDatatype :: (MonadIO m, MonadMask m) => Attribute -> (Datatype -> m a) -> m a
withAttributeDatatype attr = bracket (liftIO . h5a_get_type . unAttribute $ attr) close

checkAttributeDatatype :: (MonadIO m, MonadMask m) => Attribute -> Datatype -> m ()
checkAttributeDatatype attr dtype =
  withAttributeDatatype attr $ \dtype' ->
    when (dtype /= dtype') . throw . H5Exception (-1) [] . Just
      $! "attribute has wrong datatype: " <> show dtype' <> "; expected " <> show dtype

hasAttribute :: MonadIO m => Object t -> Text -> m Bool
hasAttribute object name = liftIO $ h5a_exists (getRawHandle object) name

deleteAttribute :: MonadIO m => Object t -> Text -> m ()
deleteAttribute object name = liftIO $ h5a_delete (getRawHandle object) name

writeAttribute :: forall a t m. (KnownDatatype' a, MonadIO m, MonadMask m) => Object t -> Text -> a -> m ()
writeAttribute object name x = do
  withDatatype' (Proxy @a) $ \dtype -> do
    exists <- hasAttribute object name
    let acquire = if exists then (openAttribute object name) else (createAttribute object name dtype)
    bracket acquire (liftIO . h5a_close . unAttribute) $ \attr -> do
      checkAttributeDatatype attr dtype
      liftIO $
        allocaForAttribute (Proxy @a) attr $ \ptr ->
          h5Poke dtype ptr x >> h5a_write (unAttribute attr) (getRawHandle dtype) ptr

allocaForAttribute :: forall a proxy b. KnownDatatype' a => proxy a -> Attribute -> (Ptr () -> IO b) -> IO b
allocaForAttribute proxy attr f = case hasStorable proxy of
  Just Dict -> alloca @a $ f . castPtr
  Nothing -> h5a_get_data_size (unAttribute attr) >>= h5Check msg >>= \n -> allocaBytes n f
    where
      msg = Just $ "failed to determine size of an attribute"

readAttribute :: forall a t m. (KnownDatatype' a, MonadIO m, MonadMask m) => Object t -> Text -> m a
readAttribute object name =
  withAttribute object name $ \attr ->
    withDatatype' (Proxy @a) $ \dtype -> do
      checkAttributeDatatype attr dtype
      liftIO $
        allocaForAttribute (Proxy @a) attr $ \ptr ->
          h5a_read (unAttribute attr) (getRawHandle dtype) ptr >> h5Peek dtype ptr
