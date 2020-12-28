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
  ( -- * Files

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

    -- | There are very few group-specific operations since groups are just objects.
    makeGroup,
    getSize,

    -- * Datasets

    -- |
    Blob (..),
    ToBlob (..),
    readDataset,
    writeDataset,
    getDatasetDims,

    -- * Datatypes
    KnownDatatype' (..),

    -- * Attributes

    -- |
    hasAttribute,
    readAttribute,
    writeAttribute,
    deleteAttribute,

    -- * Objects

    -- |
    Object (..),
    File,
    Group,
    Dataset,
    Datatype,
    FileOrGroup,
    exists,
    delete,
    getName,
    byIndex,
    byName,
    foldM,
    matchM,
    disableDiagOutput,
    H5Exception (..),

    -- * Re-exports
    IOMode (..),
    module Data.Some,
    -- | He-he-he
    dump,
  )
where

import Control.Exception.Safe hiding (handle)
import Data.ByteString (packCString, useAsCString)
import Data.Complex
import Data.Constraint (Dict (..))
import Data.HDF5.Internal
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
import Relude hiding (find, group, withFile)
import System.Directory (doesFileExist)
import System.Process (callProcess)

-- $setup
-- >>> import qualified Data.HDF5 as H5
-- >>> import Data.HDF5 (IOMode(..))

-- | Run @h5dump@ command with given arguments.
dump ::
  -- | Arguments to @h5dump@
  [Text] ->
  -- | Path to HDF5 file
  Text ->
  IO ()
dump args path = callProcess "h5dump" (toString <$> args <> [path])

--------------------------------------------------------------------------------
-- Files
--------------------------------------------------------------------------------

openFile :: MonadIO m => Text -> IOMode -> m File
openFile path mode =
  liftIO $
    doesFileExist (toString path) >>= \case
      True -> case mode of
        ReadMode -> h5f_open path c_H5F_ACC_RDONLY c_H5P_DEFAULT
        WriteMode -> h5f_create path c_H5F_ACC_TRUNC c_H5P_DEFAULT c_H5P_DEFAULT
        _ -> h5f_open path c_H5F_ACC_RDWR c_H5P_DEFAULT
      False -> case mode of
        ReadMode -> fileNotFound
        _ -> h5f_create path c_H5F_ACC_EXCL c_H5P_DEFAULT c_H5P_DEFAULT
  where
    fileNotFound = throw . H5Exception (-1) [] . Just $ "file " <> show path <> " not found"

-- | Do some work with a file.
--
-- Here's an example creating a new file:
-- >>> :{
--   H5.withFile "empty.h5" WriteMode $ \file -> do
--     n <- H5.getSize file
--     if n == 0 then putStrLn "file is empty"
--               else putStrLn "file is non-empty"
-- :}
-- file is empty
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
-- type family FileOrGroup (t :: ObjectType) :: Constraint where
--   FileOrGroup 'FileTy = ()
--   FileOrGroup 'GroupTy = ()
--   FileOrGroup t =
--     GHC.TypeLits.TypeError
--       ( 'GHC.TypeLits.Text "Expected either a File or a Group, but got "
--           'GHC.TypeLits.:<>: 'GHC.TypeLits.ShowType t
--       )
type FileOrGroup (t :: ObjectType) = OneOf '[ 'FileTy, 'GroupTy] t

type family ShowObjectType (t :: ObjectType) where
  ShowObjectType 'FileTy = 'GHC.TypeLits.Text "File"
  ShowObjectType 'GroupTy = 'GHC.TypeLits.Text "Group"
  ShowObjectType 'DatasetTy = 'GHC.TypeLits.Text "Dataset"
  ShowObjectType 'DatatypeTy = 'GHC.TypeLits.Text "Datatype"

type family ErrorHelper (t :: ObjectType) :: Constraint where
  ErrorHelper t =
    GHC.TypeLits.TypeError
      ( 'GHC.TypeLits.Text "Expected [...], but got "
          'GHC.TypeLits.:<>: ShowObjectType t
      )

type family OneOf (xs :: [ObjectType]) (x :: ObjectType) :: Constraint where
  OneOf '[] x = ErrorHelper x
  OneOf (x ': ys) x = ()
  OneOf (y ': ys) x = OneOf ys x

getRawHandle :: Object t -> Hid
getRawHandle (File h) = h
getRawHandle (Group h) = h
getRawHandle (Dataset h) = h
getRawHandle (Datatype h) = h

_getObjectType :: MonadIO m => Hid -> m H5O_type_t
_getObjectType = liftIO . h5o_get_type

close :: MonadIO m => Object t -> m ()
close = \case
  (File h) -> liftIO . h5f_close $ h
  object -> liftIO . h5o_close . getRawHandle $ object

-- | Obtain number of elements in a group (or file).
--
-- This function is not recursive! It only counts immediate children. Thus if we
-- have the following file:
--
-- >>> dump ["-H"] "example/test.h5"
-- HDF5 "example/test.h5" {
-- GROUP "/" {
--    GROUP "g" {
--       GROUP "A" {
--       }
--       DATASET "dataset1" {
--          DATATYPE  H5T_STD_I64LE
--          DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
--       }
--    }
-- }
-- }
-- >>> :{
--   H5.withFile "example/test.h5" ReadMode $ \file -> do
--     n₁ <- H5.getSize file
--     n₂ <- H5.byName file "g" . matchM @Group $ H5.getSize
--     print n₁
--     print n₂
-- :}
-- 1
-- 2
--
-- 'getSize' only considers @"g"@ as immediade child of @/@.
getSize :: (MonadIO m, FileOrGroup t) => Object t -> m Int
getSize = liftIO . h5g_get_num_objs . getRawHandle

-- | Obtain name of an object (file, group, dataset, or datatype).
--
-- This action is monadic since another thread or process can rename or delete
-- the object.
--
-- >>> :{
--   H5.withFile "example/test.h5" ReadMode $ \file ->
--     print =<< H5.byName file "/g/A" (foldSome H5.getName)
-- :}
-- "/g/A"
getName :: (HasCallStack, MonadIO m) => Object t -> m Text
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

class MatchM t where
  matchM' :: (HasCallStack, MonadThrow m) => Object t' -> m t

instance MatchM File where
  matchM' x@(File _) = return x
  matchM' x = matchFail x

instance MatchM Group where
  matchM' x@(Group _) = return x
  matchM' x = matchFail x

instance MatchM Dataset where
  matchM' x@(Dataset _) = return x
  matchM' x = matchFail x

instance MatchM Datatype where
  matchM' x@(Datatype _) = return x
  matchM' x = matchFail x

-- | Help with pattern matching on specific object types.
--
-- > withFile "my_data.h5" ReadWriteMode $ \file ->
-- >   -- Open "group1" and prove to GHC that it's a Group
-- >   file `byName` "/group1" . matchM @Group $ \g ->
-- >     getName g >>= print
matchM :: (HasCallStack, MatchM t, MonadThrow m) => (t -> m a) -> Some Object -> m a
matchM f (Some object) = matchM' object >>= f

class GetObjectType (t :: ObjectType) where
  getObjectType :: proxy t -> ObjectType

instance GetObjectType 'FileTy where getObjectType _ = FileTy

instance GetObjectType 'GroupTy where getObjectType _ = GroupTy

instance GetObjectType 'DatasetTy where getObjectType _ = DatasetTy

instance GetObjectType 'DatatypeTy where getObjectType _ = DatatypeTy

getObjectType' :: Object t -> ObjectType
getObjectType' (File _) = FileTy
getObjectType' (Group _) = GroupTy
getObjectType' (Dataset _) = DatasetTy
getObjectType' (Datatype _) = DatatypeTy

matchFail :: forall t' t m. (HasCallStack, GetObjectType t) => Object t' -> m (Object t)
matchFail x = error $ "wrong object type: " <> show t' <> "; expected " <> show t
  where
    t' = getObjectType' x
    t = getObjectType (Proxy @t)

_constructObject :: (MonadIO m, MonadMask m) => Hid -> m (Some Object)
_constructObject h =
  flip onException (liftIO $ h5o_close h) $
    _getObjectType h >>= \case
      H5O_TYPE_GROUP -> return . Some . Group $ h
      H5O_TYPE_DATASET -> return . Some . Dataset $ h
      H5O_TYPE_NAMED_DATATYPE -> return . Some . Datatype $ h
      _ -> h5Fail (Just "unknown object type") (-1 :: Int)

openByIndex :: (MonadIO m, MonadMask m, FileOrGroup t) => Object t -> Int -> m (Some Object)
openByIndex parent i = do
  n <- getSize parent
  when (i < 0 || i >= n) . error $ "invalid index: " <> show i
  r <- liftIO $ h5o_open_by_idx h "." H5_INDEX_NAME H5_ITER_INC i c_H5P_DEFAULT
  _constructObject r
  where
    !h = getRawHandle parent

openByName :: (MonadIO m, MonadMask m) => Object t -> Text -> m (Some Object)
openByName parent path = do
  r <- (h5Check msg =<<) . liftIO $ h5o_open (getRawHandle parent) path c_H5P_DEFAULT
  _constructObject r
  where
    msg = Just $ "error opening object at path " <> show path

-- | Access object at specific index.
byIndex :: (MonadIO m, MonadMask m, FileOrGroup t) => Object t -> Int -> (Some Object -> m a) -> m a
byIndex g i = bracket (openByIndex g i) (\(Some x) -> close x)

byName :: (MonadIO m, MonadMask m, FileOrGroup t) => Object t -> Text -> (Some Object -> m a) -> m a
byName object path = bracket (openByName object path) (\(Some x) -> close x)

foldM :: forall a t m. (MonadIO m, MonadMask m, FileOrGroup t) => (a -> Some Object -> m a) -> a -> Object t -> m a
foldM !f !acc₀ = \g -> go g 0 acc₀
  where
    go :: Object t -> Int -> a -> m a
    go !group !i !acc =
      getSize group >>= \n ->
        if i < n
          then byIndex group i (f acc) >>= go group (i + 1)
          else return acc

delete :: MonadIO m => Object t -> Text -> m ()
delete parent path = liftIO $ h5l_delete (getRawHandle parent) path c_H5P_DEFAULT

withRoot :: (MonadIO m, MonadMask m) => Object t -> (Group -> m a) -> m a
withRoot x@(File _) func = x `byName` "/" $ \case
  Some g@(Group _) -> func g
  _ -> error "by construction, this cannot happen"
withRoot x func = bracket (liftIO . h5i_get_file_id . getRawHandle $ x) close $ \f ->
  withRoot f func

exists :: (FileOrGroup t, MonadIO m, MonadMask m) => Object t -> Text -> m Bool
exists parent path
  | T.null path = return True
  | T.head path == '/' = withRoot parent $ \g -> exists g (T.tail path)
  | otherwise = existsHelper parent (T.split (== '/') path)
  where
    existsHelper :: (FileOrGroup t, MonadIO m, MonadMask m) => Object t -> [Text] -> m Bool
    existsHelper _ [] = return True
    existsHelper p (first' : rest) = do
      linkExists p first' >>= \case
        True -> p `byName` first' $ \(Some object) ->
          case object of
            x@(Group _) -> existsHelper x rest
            _ -> return (null rest)
        False -> return False
    linkExists :: MonadIO m => Object t -> Text -> m Bool
    linkExists p name = liftIO $ h5l_exists (getRawHandle p) name c_H5P_DEFAULT

--------------------------------------------------------------------------------
-- Groups
--------------------------------------------------------------------------------

makeGroup :: MonadIO m => Object t -> Text -> m ()
makeGroup parent path = create >>= close
  where
    create = liftIO $ h5g_create (getRawHandle parent) path c_H5P_DEFAULT c_H5P_DEFAULT c_H5P_DEFAULT

--------------------------------------------------------------------------------
-- Datasets
--------------------------------------------------------------------------------

data Blob a where
  Blob :: (Storable a, KnownDatatype' a) => ![Int] -> {-# UNPACK #-} !(Vector a) -> Blob a

deriving stock instance Show a => Show (Blob a)

class (Storable b, KnownDatatype' b) => ToBlob a b | a -> b where
  toBlob :: a -> Blob b

instance (Storable a, KnownDatatype' a) => ToBlob (Blob a) a where
  toBlob = id

instance (Storable a, KnownDatatype' a) => ToBlob (Vector a) a where
  toBlob v = Blob [V.length v] v

instance (Storable a, KnownDatatype' a) => ToBlob [a] a where
  toBlob xs = Blob [V.length v] v
    where
      v = V.fromList xs

getDatasetDims :: MonadIO m => Dataset -> m [Int]
getDatasetDims dataset =
  liftIO $
    withDatasetDataspace dataset $ \dataspace -> do
      nDims <- fromIntegral <$> h5s_get_simple_extent_ndims (unDataspace dataspace)
      allocaArray nDims $ \dimsPtr -> do
        h5s_get_simple_extent_dims (unDataspace dataspace) dimsPtr nullPtr
        fmap fromIntegral <$> peekArray nDims dimsPtr

checkDatasetDatatype :: (MonadIO m, MonadMask m) => Dataset -> Datatype -> m ()
checkDatasetDatatype dataset dtype =
  withDatasetDatatype dataset $ \dtype' ->
    when (dtype /= dtype') . throw . H5Exception (-1) [] . Just
      $! "dataset has wrong datatype: " <> show dtype' <> "; expected " <> show dtype

withDatasetDatatype :: (MonadIO m, MonadMask m) => Dataset -> (Datatype -> m a) -> m a
withDatasetDatatype object = bracket (liftIO . h5d_get_type . getRawHandle $ object) close

readDataset :: forall a m. (MonadIO m, MonadMask m, Storable a, KnownDatatype' a) => Dataset -> m (Blob a)
readDataset dataset = do
  dims <- getDatasetDims dataset
  withDatatype' (Proxy @a) $ \dtype -> do
    checkDatasetDatatype dataset dtype
    v <- liftIO $ do
      buffer <- MV.unsafeNew (product dims)
      MV.unsafeWith buffer $ \ptr ->
        h5d_read (getRawHandle dataset) (getRawHandle dtype) c_H5S_ALL c_H5S_ALL c_H5P_DEFAULT (castPtr ptr)
      V.unsafeFreeze buffer
    return $ Blob dims v

createDataset ::
  forall a t m.
  (MonadIO m, MonadMask m, KnownDatatype' a) =>
  Object t ->
  Text ->
  Blob a ->
  m Dataset
createDataset object name (Blob dims _) =
  withDatatype' (Proxy @a) $ \dtype ->
    withDataspace H5S_SIMPLE $ \dataspace -> liftIO $ do
      withArrayLen (fromIntegral <$> dims) $ \rank dimsPtr ->
        h5s_set_extent_simple (unDataspace dataspace) rank dimsPtr dimsPtr
      h5d_create2
        (getRawHandle object)
        name
        (getRawHandle dtype)
        (unDataspace dataspace)
        c_H5P_DEFAULT
        c_H5P_DEFAULT
        c_H5P_DEFAULT

withDataset ::
  forall a t b m.
  (FileOrGroup t, MonadIO m, MonadMask m, KnownDatatype' a) =>
  Object t ->
  Text ->
  Blob a ->
  (Dataset -> m b) ->
  m b
withDataset object name blob f =
  exists object name >>= \case
    True -> byName object name $ \case
      Some x@(Dataset _) -> f x
      Some x -> do
        t <- _getObjectType (getRawHandle x)
        let msg =
              "object "
                <> show name
                <> " has wrong type: "
                <> show t
                <> "; expected a "
                <> show H5O_TYPE_DATASET
        h5Fail (Just msg) (-1 :: Int)
    False -> bracket (createDataset object name blob) close f

writeDataset :: forall a b t m. (FileOrGroup t, MonadIO m, MonadMask m, ToBlob b a) => Object t -> Text -> b -> m ()
writeDataset object name tensor =
  withDataset object name blob $ \dataset -> do
    dims' <- getDatasetDims dataset
    when (dims' /= dims) $
      let msg = "dataset has wrong size: " <> show dims' <> "; expected " <> show dims
       in h5Fail (Just msg) (-1 :: Int)
    withDatatype' (Proxy @a) $ \dtype -> do
      checkDatasetDatatype dataset dtype
      liftIO $
        V.unsafeWith v $ \ptr ->
          h5d_write (getRawHandle dataset) (getRawHandle dtype) c_H5S_ALL c_H5S_ALL c_H5P_DEFAULT (castPtr ptr)
  where
    blob@(Blob dims v) = toBlob tensor

--------------------------------------------------------------------------------
-- Datatypes
--------------------------------------------------------------------------------

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
    size = sizeOf' (Proxy @a)

withTextDatatype :: (MonadIO m, MonadMask m) => (Datatype -> m a) -> m a
withTextDatatype = bracket (liftIO acquire) close
  where
    acquire = do
      dtype <- h5t_copy =<< peek h5t_C_S1
      flip onException (close dtype) $ do
        h5t_set_cset (getRawHandle dtype) H5T_CSET_UTF8
        h5t_set_size (getRawHandle dtype) c_H5T_VARIABLE
      return dtype

-- | Specifies size of a 'Datatype'.
--
-- We want to support text attributes which usually have variable size. Hence,
-- just adding 'Storable' constraint is not a solution.
-- data DatatypeSize
--   = FixedSize {-# UNPACK #-} !Int
--   | VariableSize
class KnownDatatype' a where
  withDatatype' :: (MonadIO m, MonadMask m) => proxy a -> (Datatype -> m b) -> m b

  hasStorable :: proxy a -> Maybe (Dict (Storable a))
  hasStorable _ = Nothing

  h5Peek :: Ptr () -> Int -> IO a
  default h5Peek :: Storable a => Ptr () -> Int -> IO a
  h5Peek p n
    | n == (sizeOf' (Proxy @a)) = peek $ castPtr p
    | otherwise =
      error $
        "invalid buffer size: " <> show n <> "; expected " <> show (sizeOf' (Proxy @a))
  h5With :: a -> (Ptr () -> Int -> IO b) -> IO b
  default h5With :: Storable a => a -> (Ptr () -> Int -> IO b) -> IO b
  h5With x f = with x $ \buffer -> f (castPtr buffer) (sizeOf x)

instance KnownDatatype' Int where
  withDatatype' _ = fromStaticPointer h5t_NATIVE_INT64
  hasStorable _ = Just Dict

instance KnownDatatype' Word64 where
  withDatatype' _ = fromStaticPointer h5t_NATIVE_UINT64
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

instance KnownDatatype' Text where
  withDatatype' _ = withTextDatatype
  h5Peek p n
    | n == (sizeOf' (Proxy @(Ptr ()))) =
      peek (castPtr p :: Ptr (Ptr CChar)) >>= \p' ->
        decodeUtf8 <$> packCString p'
    | otherwise =
      error $
        "expected a buffer of length "
          <> show (sizeOf' (Proxy @(Ptr ())))
          <> " for variable-length string"
  h5With x f = useAsCString (encodeUtf8 x) $ \p ->
    with p $ \p' -> f (castPtr p') 1

fromStaticPointer :: MonadIO m => Ptr Hid -> (Datatype -> m b) -> m b
fromStaticPointer p f = f . Datatype =<< liftIO (peek p)

--------------------------------------------------------------------------------
-- Dataspaces
--------------------------------------------------------------------------------

newtype Dataspace = Dataspace {unDataspace :: Hid}

withDataspace :: (MonadIO m, MonadMask m) => H5S_class_t -> (Dataspace -> m a) -> m a
withDataspace c =
  bracket (liftIO $ Dataspace <$> h5s_create c) (liftIO . h5s_close . unDataspace)

withDatasetDataspace :: (MonadIO m, MonadMask m) => Dataset -> (Dataspace -> m a) -> m a
withDatasetDataspace dataset =
  bracket
    (liftIO $ Dataspace <$> h5d_get_space (getRawHandle dataset))
    (liftIO . h5s_close . unDataspace)

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------

newtype Attribute = Attribute {unAttribute :: Hid}

openAttribute :: MonadIO m => Object t -> Text -> m Attribute
openAttribute object name = liftIO $ Attribute <$> h5a_open (getRawHandle object) name c_H5P_DEFAULT

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
          c_H5P_DEFAULT
          c_H5P_DEFAULT

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
    exists' <- hasAttribute object name
    let acquire = if exists' then (openAttribute object name) else (createAttribute object name dtype)
    bracket acquire (liftIO . h5a_close . unAttribute) $ \attr -> do
      checkAttributeDatatype attr dtype
      liftIO $ h5With x $ \ptr _ -> h5a_write (unAttribute attr) (getRawHandle dtype) ptr

allocaForDatatype :: forall a proxy b. KnownDatatype' a => proxy a -> Datatype -> (Ptr () -> Int -> IO b) -> IO b
allocaForDatatype proxy dtype f = case hasStorable proxy of
  Just Dict -> alloca $ \(ptr :: Ptr a) -> f (castPtr ptr) (sizeOf' (Proxy @a))
  Nothing -> h5t_get_size (getRawHandle dtype) >>= \n -> allocaBytes n $ \ptr -> f ptr n

-- allocaForAttribute :: forall a proxy b. KnownDatatype' a => proxy a -> Attribute -> (Ptr () -> Int -> IO b) -> IO b
-- allocaForAttribute proxy attr f = case hasStorable proxy of
--   Just Dict -> alloca @a $ \ptr -> f (castPtr ptr) (sizeOf (undefined :: a))
--   Nothing -> withDatatype' (Proxy @a) $ \dtype ->
--     h5t_get_size (getRawHandle dtype) >>= \n -> allocaBytes n $ \ptr -> f ptr n

-- | Read an attribute
readAttribute :: forall a t m. (KnownDatatype' a, MonadIO m, MonadMask m) => Object t -> Text -> m a
readAttribute object name =
  withAttribute object name $ \attr ->
    withDatatype' (Proxy @a) $ \dtype -> do
      checkAttributeDatatype attr dtype
      liftIO $
        allocaForDatatype (Proxy @a) dtype $ \ptr n ->
          h5a_read (unAttribute attr) (getRawHandle dtype) ptr >> h5Peek ptr n

sizeOf' :: forall a proxy. Storable a => proxy a -> Int
sizeOf' _ = sizeOf (undefined :: a)
