{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.HDF5.Wrapper (
  version,
  disableDiagOutput,
  runHDF5,

  -- * Files
  openFile,
  h5f_close,
  AccessFlags (..),
  -- h5f_get_filesize,

  -- * Objects
  open,
  openByIndex,
  open',
  openByIndex',
  h5o_close,
  h5o_open,
  -- h5o_open_by_idx,

  -- * Groups
  h5g_get_num_objs,
  h5g_create,
  createGroup,
  groupSize,
  mapGroupM,
  forGroupM,

  -- * Links

  -- h5l_iterate,
  h5l_delete,
  h5l_exists,

  -- * Identifiers
  getName,
  h5i_get_name,
  h5i_get_file_id,
  h5i_get_type,

  -- * Datatypes
  datatypeEqual,
  datatypeSize,
  datatypeName,
  checkDatatype,

  -- * Dataspaces
  simpleDataspace,
  dataspaceRank,
  dataspaceShape,
  toHyperslab,
  sliceHyperslab,
  selectHyperslab,
  prepareStrides,
  slice,
  sliceWithHyperslab,
  toSelection,
  sliceDataset,
  boundingBox,
  readDataset,
  readSelected,
  readInplace,
  readSelectedInplace,
  writeDataset,
  writeSelected,
  createEmptyDataset,
  arrayViewDataspace,
  dataspaceSelectionType,
  SelectionType (..),

  -- * Datasets
  h5d_open,
  h5d_create,
  h5d_get_space,
  h5d_get_type,
  -- h5d_read,
  -- h5d_write,
  TemporaryContiguousArray (..),
  TemporaryStridedMatrix (..),
  Scalar (..),
  getDataspace,
  datasetRank,
  datasetShape,

  -- * Attributes
  readAttribute,
  writeAttribute,
  existsAttribute,
  deleteAttribute,
  attributeDatatype,
  attributeDataspace,

  -- * Helpers
  checkError,
  fromHtri,
  H5Exception (..),
)
where

import Control.Exception.Safe
import Control.Monad.IO.Unlift
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8
import Data.ByteString.Internal (ByteString (..))
import Data.Complex
import Data.HDF5.Context
import Data.HDF5.Types
import qualified Data.List
import Data.Some
import Data.Typeable (eqT, (:~:) (..))
import Data.Vector.Storable (Vector, (!))
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.ByteString
import Data.Vector.Storable.Mutable (MVector (..))
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.C.String (CString)
import Foreign.C.Types (CChar, CDouble, CFloat, CUInt)
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr, mallocForeignPtr, mallocForeignPtrBytes, newForeignPtr_, withForeignPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes, free, mallocBytes)
import Foreign.Marshal.Array (allocaArray, peekArray, pokeArray, withArrayLen)
import Foreign.Marshal.Utils (toBool)
import Foreign.Ptr (FunPtr, Ptr, castPtr, freeHaskellFunPtr, nullPtr, plusPtr)
import Foreign.Storable
import qualified GHC.Show
import GHC.Stack
import GHC.TypeLits
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import ListT (ListT)
import qualified ListT
import System.Directory (doesFileExist)
import qualified System.IO.Unsafe
import UnliftIO.Resource
import Prelude hiding (Handle, first, group)

C.context (C.baseCtx <> C.bsCtx <> C.funCtx <> h5Ctx)
C.include "<hdf5.h>"
C.include "<hdf5_hl.h>"

version :: (Int, Int, Int)
version = System.IO.Unsafe.unsafePerformIO $ do
  alloca $ \aPtr ->
    alloca $ \bPtr ->
      alloca $ \cPtr -> do
        !_ <-
          checkError
            =<< [CU.exp| herr_t { H5get_libversion($(unsigned* aPtr), $(unsigned* bPtr),
                                                   $(unsigned* cPtr)) } |]
        (,,)
          <$> (fromIntegral <$> peek aPtr)
          <*> (fromIntegral <$> peek bPtr)
          <*> (fromIntegral <$> peek cPtr)

----------------------------------------------------------------------------------------------------
-- Error handling
----------------------------------------------------------------------------------------------------
-- {{{

class CheckError' a b | a -> b where
  checkError' :: HasCallStack => a -> b

instance CheckError' Hid Hid where
  checkError' !x
    | x < 0 = error $ "HDF5 failed with error code " <> show x
    | otherwise = x

instance CheckError' Herr () where
  checkError' !x
    | x < 0 = error $ "HDF5 failed with error code " <> show x
    | otherwise = ()

disableDiagOutput :: IO ()
disableDiagOutput = checkError' <$> [CU.exp| herr_t { H5Eset_auto(H5E_DEFAULT, NULL, NULL) } |]

data ErrorInfo = ErrorInfo !Text !Text !Int !Text

fromCString :: CString -> IO Text
fromCString p = decodeUtf8 <$> B.packCString p

-- @
-- typedef struct H5E_error2_t {
--     hid_t       cls_id;    /*class ID                           */
--     hid_t       maj_num;   /*major error ID		     */
--     hid_t       min_num;   /*minor error number		     */
--     unsigned    line;      /*line in file where error occurs    */
--     const char *func_name; /*function in which error occurred   */
--     const char *file_name; /*file in which error occurred       */
--     const char *desc;      /*optional supplied description      */
-- } H5E_error2_t;
-- @
peekErrorInfo :: Ptr H5E_error2_t -> IO ErrorInfo
peekErrorInfo p =
  ErrorInfo
    <$> (fromCString =<< [CU.exp| const char* { $(H5E_error2_t* p)->file_name } |])
    <*> (fromCString =<< [CU.exp| const char* { $(H5E_error2_t* p)->func_name } |])
    <*> (fromIntegral <$> [CU.exp| unsigned int { $(H5E_error2_t* p)->line } |])
    <*> (fromCString =<< [CU.exp| const char* { $(H5E_error2_t* p)->desc } |])

prettyErrorInfo :: ErrorInfo -> Text
prettyErrorInfo (ErrorInfo file func line msg) =
  file <> ":" <> show line <> ":" <> func <> "(): " <> msg

type H5E_walk2_t = CUInt -> Ptr H5E_error2_t -> Ptr () -> IO Herr

foreign import ccall "wrapper"
  mkWalk :: H5E_walk2_t -> IO (FunPtr H5E_walk2_t)

collectStack :: HasCallStack => IO [ErrorInfo]
collectStack = uninterruptibleMask_ $
  bracket h5e_get_current_stack h5e_close_stack $ \stackId -> do
    (listRef :: IORef [ErrorInfo]) <- newIORef []
    let callback _ ptr _ = peekErrorInfo ptr >>= \info -> modifyIORef listRef ((:) info) >> return 0
    bracket (mkWalk callback) freeHaskellFunPtr $ \fn -> do
      h5e_walk2 stackId fn
    readIORef listRef
 where
  h5e_get_current_stack :: HasCallStack => IO Hid
  h5e_get_current_stack = checkError' <$> [CU.exp| hid_t { H5Eget_current_stack() } |]
  h5e_close_stack :: HasCallStack => Hid -> IO ()
  h5e_close_stack x = checkError' <$> [CU.exp| herr_t { H5Eclose_stack($(hid_t x)) } |]
  h5e_walk2 :: HasCallStack => Hid -> FunPtr H5E_walk2_t -> IO ()
  h5e_walk2 stack func =
    fmap checkError' $
      [C.exp| herr_t {
          H5Ewalk2($(hid_t stack),
                   H5E_WALK_DOWNWARD,
                   $(herr_t (* func)(unsigned int, const H5E_error2_t*, void*)),
                   NULL) } |]

data H5Exception where
  H5Exception :: HasCallStack => !Int -> ![ErrorInfo] -> !(Maybe Text) -> H5Exception

deriving anyclass instance Exception H5Exception

instance Show H5Exception where
  show = toString . prettyH5Exception

prettyH5Exception :: H5Exception -> Text
prettyH5Exception (H5Exception code stack msg) =
  "HDF5 error "
    <> show code
    <> msg'
    <> stack'
    <> "\n"
    <> toText (prettyCallStack callStack)
 where
  msg' = maybe "" (": " <>) msg
  stack' = case (intersperse "\n  " $ prettyErrorInfo <$> stack) of
    xs@(_ : _) -> mconcat $ "\n  " : xs
    [] -> ""

_fail :: (HasCallStack, Integral a) => Maybe Text -> a -> IO a
_fail msg code = collectStack >>= \stack -> throw $ H5Exception (fromIntegral code) stack msg

_checkError :: (HasCallStack, Integral a) => Maybe Text -> a -> IO a
_checkError msg !x
  | x < 0 = _fail msg x
  | otherwise = return x

-- checkErrorWithMsg :: Integral a => Text -> a -> IO a
-- checkErrorWithMsg msg = _checkError (Just msg)

checkError :: (HasCallStack, Integral a, MonadIO m) => a -> m a
checkError = liftIO . _checkError Nothing

checkError_ :: (HasCallStack, Integral a, MonadIO m) => a -> m ()
checkError_ x = do
  !_ <- checkError x
  pure ()

-- }}}
----------------------------------------------------------------------------------------------------
-- ResourceT
----------------------------------------------------------------------------------------------------
runHDF5 :: (NFData a, MonadUnliftIO m) => ResourceT m a -> m a
runHDF5 action = runResourceT $ do
  !r <- force <$> action
  pure r

----------------------------------------------------------------------------------------------------
-- Files
----------------------------------------------------------------------------------------------------
-- {{{
data AccessFlags
  = ReadOnly
  | WriteTruncate
  | WriteAppend
  deriving stock (Read, Show, Eq)

accessFlagsToUInt :: AccessFlags -> CUInt
accessFlagsToUInt ReadOnly = [CU.pure| unsigned int { H5F_ACC_RDONLY } |]
accessFlagsToUInt WriteTruncate = [CU.pure| unsigned int { H5F_ACC_TRUNC } |]
accessFlagsToUInt WriteAppend = [CU.pure| unsigned int { H5F_ACC_RDWR } |]

createHandle :: MonadResource m => (Hid -> IO ()) -> IO Hid -> m Handle
createHandle cleanup acquire =
  allocate (acquire >>= checkError) cleanup >>= \case
    (k, v) -> pure $ Handle v k

openFile ::
  (HasCallStack, MonadResource m) =>
  -- | filename
  Text ->
  -- | mode in which to open the file
  AccessFlags ->
  -- | file handle
  m File
openFile filename@(encodeUtf8 -> c_filename) flags@(accessFlagsToUInt -> c_flags) =
  fmap File $
    case flags of
      ReadOnly -> open
      WriteTruncate -> create
      WriteAppend ->
        liftIO (doesFileExist (toString filename)) >>= \case
          True -> open
          False -> create
 where
  open =
    createHandle
      h5f_close
      [CU.exp| hid_t { H5Fopen($bs-cstr:c_filename, $(unsigned int c_flags), H5P_DEFAULT) } |]
  create =
    createHandle
      h5f_close
      [CU.exp| hid_t { H5Fcreate($bs-cstr:c_filename, $(unsigned int c_flags), H5P_DEFAULT, H5P_DEFAULT) } |]

h5f_close :: HasCallStack => Hid -> IO ()
h5f_close h = checkError_ =<< [CU.exp| herr_t { H5Fclose($(hid_t h)) } |]

{- | Get size of a file.
 h5f_get_filesize ::
   HasCallStack =>
   -- | File handle
   Hid ->
   -- | File size in bytes
   IO Int
 h5f_get_filesize file =
   fmap fromIntegral . checkError
     =<< [CU.block| int64_t {
           hsize_t size;
           herr_t status = H5Fget_filesize($(hid_t file), &size);
           return status < 0 ? (int64_t)status : (int64_t)size;
         } |]
-}

-- }}}
----------------------------------------------------------------------------------------------------
-- Objects
----------------------------------------------------------------------------------------------------
-- {{{

-- | Close an object.
h5o_close :: HasCallStack => Hid -> IO ()
h5o_close h = void . checkError =<< [CU.exp| herr_t { H5Oclose($(hid_t h)) } |]

-- | Open an object by name.
h5o_open ::
  HasCallStack =>
  -- | @parent@ file or group
  Hid ->
  -- | path to object relative to @parent@.
  Text ->
  -- | new object handle
  IO Hid
h5o_open parent path =
  withFrozenCallStack $
    checkError =<< [CU.exp| hid_t { H5Oopen($(hid_t parent), $bs-cstr:c_path, H5P_DEFAULT) } |]
 where
  c_path = encodeUtf8 path

-- | Open an object by index.
h5o_open_by_idx ::
  HasCallStack =>
  -- | @parent@ file or group
  Hid ->
  -- | index in @parent@
  Int ->
  -- | new object handle
  IO Hid
h5o_open_by_idx parent index
  | index < 0 = error $ "invalid index: " <> show index
  | otherwise =
      withFrozenCallStack $
        checkError
          =<< [CU.exp| hid_t { H5Oopen_by_idx($(hid_t parent), ".", H5_INDEX_NAME,
                                            H5_ITER_INC, $(hsize_t c_index), H5P_DEFAULT) } |]
 where
  c_index = fromIntegral index

openObjectRaw :: (HasCallStack, MonadIO m) => Handle -> m (Some Object)
openObjectRaw h@(Handle raw _) = do
  liftIO (h5i_get_type raw) >>= \case
    H5I_FILE -> return . mkSome . File $ h
    H5I_GROUP -> return . mkSome . Group $ h
    H5I_DATASET -> return . mkSome . Dataset $ h
    H5I_DATATYPE -> return . mkSome . Datatype $ h
    t -> do
      name <- liftIO $ h5i_get_name raw
      error $ show name <> " is a " <> show t <> "; expected an Object"

objectType :: Object t -> ObjectType
objectType object = case object of
  (File _) -> FileTy
  (Group _) -> GroupTy
  (Dataset _) -> DatasetTy
  (Datatype _) -> DatatypeTy

prettyObjectType :: ObjectType -> Text
prettyObjectType tp = case tp of
  FileTy -> "File"
  GroupTy -> "Group"
  DatasetTy -> "Dataset"
  DatatypeTy -> "Datatype"

wrongObjectType :: HasCallStack => Object t1 -> proxy (Object t2) -> a
wrongObjectType object _ =
  error $
    "wrong object type: "
      <> show (getName object)
      <> " is a "
      <> prettyObjectType (objectType object)

cast :: forall from to. (HasCallStack, Typeable from, Typeable to) => Object from -> Object to
cast x = case (eqT :: Maybe (Object to :~: Object from)) of
  Just Refl -> coerce x
  Nothing -> withFrozenCallStack $ wrongObjectType x (Proxy :: Proxy (Object to))

cast' :: forall t. (HasCallStack, Typeable t) => Some Object -> Object t
cast' = foldSome helper
 where
  helper :: Object k -> Object t
  helper = \case
    x@(File _) -> cast x
    x@(Group _) -> cast x
    x@(Dataset _) -> cast x
    x@(Datatype _) -> cast x

open' :: (HasCallStack, MonadResource m) => Group -> Text -> m (Some Object)
open' parent path =
  allocate (liftIO $ h5o_open (rawHandle parent) path) (liftIO . h5o_close) >>= \case
    (k, v) -> openObjectRaw (Handle v k)

openByIndex' :: (HasCallStack, MonadResource m) => Group -> Int -> m (Some Object)
openByIndex' parent index =
  allocate (liftIO $ h5o_open_by_idx (rawHandle parent) index) (liftIO . h5o_close) >>= \case
    (k, v) -> openObjectRaw (Handle v k)

open :: (HasCallStack, Typeable t, MonadResource m) => Group -> Text -> m (Object t)
open parent path = open' parent path >>= return . withFrozenCallStack cast'

openByIndex :: (HasCallStack, Typeable t, MonadResource m) => Group -> Int -> m (Object t)
openByIndex parent index = openByIndex' parent index >>= return . withFrozenCallStack cast'

-- }}}
----------------------------------------------------------------------------------------------------
-- Groups
----------------------------------------------------------------------------------------------------
-- {{{

-- | Get number of objects in a group.
h5g_get_num_objs ::
  -- | file or group handle
  Hid ->
  -- | number of objects
  IO Int
h5g_get_num_objs h =
  fmap fromIntegral . checkError
    =<< [CU.block| int64_t {
          H5G_info_t group_info;
          herr_t status = H5Gget_info($(hid_t h), &group_info);
          return status < 0 ? (int64_t)status : (int64_t)group_info.nlinks;
        } |]

groupSize :: (HasCallStack, MonadIO m) => Group -> m Int
groupSize g = liftIO $ h5g_get_num_objs (rawHandle g)

mapGroupM :: MonadResource m => (forall t. Object t -> m a) -> Group -> ListT m a
mapGroupM action g = do
  let f !i = do
        n <- groupSize g
        if i < n
          then do
            r <- foldSome action =<< openByIndex' g i
            pure $ Just (r, i + 1)
          else pure Nothing
  ListT.unfoldM f 0

forGroupM :: MonadResource m => Group -> (forall t. Object t -> m a) -> ListT m a
forGroupM g action = mapGroupM action g

-- | Create a new group.
h5g_create ::
  HasCallStack =>
  -- | @parent@ file or group
  Hid ->
  -- | absolute or relative path to the new group
  Text ->
  IO ()
h5g_create parent path = do
  !_ <-
    checkError
      =<< [CU.block| herr_t {
            const hid_t g = H5Gcreate($(hid_t parent), $bs-cstr:c_path,
                                      H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            if (g < 0) { return (herr_t)g; }
            return H5Gclose(g);
          } |]
  return ()
 where
  c_path = encodeUtf8 path

createGroup :: (HasCallStack, MonadResource m) => Group -> Text -> m Group
createGroup parent path = do
  liftIO $ h5g_create (rawHandle parent) path
  open parent path

-- }}}
----------------------------------------------------------------------------------------------------
-- Links
----------------------------------------------------------------------------------------------------
-- {{{
-- type H5L_iterate_t = Hid -> Ptr CChar -> Ptr H5L_info_t -> Ptr () -> IO Herr
--
-- foreign import ccall "wrapper"
--   mkH5L_iterate_t :: H5L_iterate_t -> IO (FunPtr H5L_iterate_t)

{- | Iterate over immediate children of an object, think 'forM_' but for HDF5
 groups.
 h5l_iterate ::
   HasCallStack =>
   -- | parent file or group
   Hid ->
   -- | action to perform on every object
   (Hid -> Text -> IO ()) ->
   IO ()
 h5l_iterate group action = do
   let helper h c_name _ _ = fromCString c_name >>= action h >> return 0
   bracket (mkH5L_iterate_t helper) freeHaskellFunPtr $ \actionPtr ->
     void . checkError
       =<< [C.exp| herr_t {
             H5Literate1($(hid_t group),
                         H5_INDEX_NAME,
                         H5_ITER_INC,
                         NULL,
                         $(herr_t (* actionPtr)(hid_t, const char*, const H5L_info_t*, void*)),
                         NULL)
           } |]
-}

-- | Delete a link.
h5l_delete ::
  HasCallStack =>
  -- | parent file or group
  Hid ->
  -- | link path
  Text ->
  IO ()
h5l_delete parent path = do
  !_ <-
    checkError
      =<< [CU.exp| herr_t { H5Ldelete($(hid_t parent), $bs-cstr:c_path, H5P_DEFAULT) } |]
  return ()
 where
  c_path = encodeUtf8 path

-- | Check whether a link exists
h5l_exists ::
  -- | parent file or group
  Hid ->
  -- | link path
  Text ->
  -- | whether the link exists
  IO Bool
h5l_exists parent path =
  toBool
    <$> [CU.exp| htri_t { H5Lexists($(hid_t parent), $bs-cstr:c_path, H5P_DEFAULT) } |]
 where
  c_path = encodeUtf8 path

-- }}}
----------------------------------------------------------------------------------------------------
-- Identifiers
----------------------------------------------------------------------------------------------------
-- {{{

-- | Get identifier name.
h5i_get_name :: HasCallStack => Hid -> IO Text
h5i_get_name h = do
  numBytes <- c_get_name h nullPtr 0
  allocaBytes (numBytes + 1) $ \s -> c_get_name h s (numBytes + 1) >> fromCString s
 where
  c_get_name :: Hid -> Ptr CChar -> Int -> IO Int
  c_get_name x buffer size =
    let c_size = fromIntegral size
     in fmap fromIntegral . checkError
          =<< [CU.exp| hssize_t { H5Iget_name($(hid_t x), $(char* buffer), $(size_t c_size)) } |]

-- | Get parent file.
h5i_get_file_id ::
  Hid ->
  -- | new file handle which must be released using 'h5f_close'
  IO Hid
h5i_get_file_id h = checkError =<< [CU.exp| hid_t { H5Iget_file_id($(hid_t h)) } |]

instance Enum H5I_type_t where
  fromEnum x = fromIntegral $ case x of
    H5I_FILE -> [CU.pure| int { H5I_FILE } |]
    H5I_GROUP -> [CU.pure| int { H5I_GROUP } |]
    H5I_DATATYPE -> [CU.pure| int { H5I_DATATYPE } |]
    H5I_DATASPACE -> [CU.pure| int { H5I_DATASPACE } |]
    H5I_DATASET -> [CU.pure| int { H5I_DATASET } |]
    H5I_ATTR -> [CU.pure| int { H5I_ATTR } |]
  toEnum x
    | toBool [CU.pure| bool { $(int c_x) == H5I_FILE } |] = H5I_FILE
    | toBool [CU.pure| bool { $(int c_x) == H5I_GROUP } |] = H5I_GROUP
    | toBool [CU.pure| bool { $(int c_x) == H5I_DATATYPE } |] = H5I_DATATYPE
    | toBool [CU.pure| bool { $(int c_x) == H5I_DATASPACE } |] = H5I_DATASPACE
    | toBool [CU.pure| bool { $(int c_x) == H5I_DATASET } |] = H5I_DATASET
    | toBool [CU.pure| bool { $(int c_x) == H5I_ATTR } |] = H5I_ATTR
    | otherwise = error $ "invalid H5I_type_t: " <> show x
   where
    c_x = fromIntegral x

-- | Get object type
h5i_get_type :: Hid -> IO H5I_type_t
h5i_get_type h = do
  fmap (toEnum . fromIntegral) $
    checkError
      =<< [CU.block| int {
            const H5I_type_t t = H5Iget_type($(hid_t h));
            return t == H5I_BADID ? -1 : t;
          } |]

getName :: HasCallStack => Object t -> Text
getName object = System.IO.Unsafe.unsafePerformIO $ h5i_get_name (rawHandle object)

-- }}}
----------------------------------------------------------------------------------------------------
-- Datatypes
----------------------------------------------------------------------------------------------------
-- {{{

getStaticDatatype :: MonadResource m => Hid -> m Datatype
getStaticDatatype p = Datatype . Handle p <$> register (return ())

h5t_NATIVE_INT32 :: Hid
h5t_NATIVE_INT32 = [CU.pure| hid_t { H5T_NATIVE_INT32 } |]

h5t_NATIVE_INT64 :: Hid
h5t_NATIVE_INT64 = [CU.pure| hid_t { H5T_NATIVE_INT64 } |]

h5t_NATIVE_UINT32 :: Hid
h5t_NATIVE_UINT32 = [CU.pure| hid_t { H5T_NATIVE_UINT32 } |]

h5t_NATIVE_UINT64 :: Hid
h5t_NATIVE_UINT64 = [CU.pure| hid_t { H5T_NATIVE_UINT64 } |]

h5t_NATIVE_FLOAT :: Hid
h5t_NATIVE_FLOAT = [CU.pure| hid_t { H5T_NATIVE_FLOAT } |]

h5t_NATIVE_DOUBLE :: Hid
h5t_NATIVE_DOUBLE = [CU.pure| hid_t { H5T_NATIVE_DOUBLE } |]

createComplexDatatype :: HasCallStack => Hid -> IO Hid
createComplexDatatype dtype =
  withFrozenCallStack $
    checkError
      =<< [CU.block| hid_t {
            const size_t size = H5Tget_size($(hid_t dtype));
            if (size == 0) { return -1; }
            hid_t complex_dtype = H5Tcreate(H5T_COMPOUND, 2 * size);
            if (complex_dtype < 0) { return complex_dtype; }
            herr_t status = H5Tinsert(complex_dtype, "r", 0, $(hid_t dtype));
            if (status < 0) { goto cleanup; }
            status = H5Tinsert(complex_dtype, "i", size, $(hid_t dtype));
            if (status < 0) { goto cleanup; }
            return complex_dtype;

          cleanup:
            H5Tclose(complex_dtype);
            return status;
          } |]

getComplexDatatype :: MonadResource m => Datatype -> m Datatype
getComplexDatatype dtype = do
  let acq = liftIO $ createComplexDatatype (rawHandle dtype)
      rel = liftIO . h5o_close
  allocate acq rel >>= \case
    (k, h) -> return $ Datatype (Handle h k)

createTextDatatype :: HasCallStack => IO Hid
createTextDatatype =
  withFrozenCallStack $
    checkError
      =<< [CU.block| hid_t {
            const hid_t dtype = H5Tcopy(H5T_C_S1);
            if (dtype < 0) { return dtype; }
            herr_t status = H5Tset_cset(dtype, H5T_CSET_UTF8);
            if (status < 0) { goto cleanup; }
            status = H5Tset_size(dtype, H5T_VARIABLE);
            if (status < 0) { goto cleanup; }
            return dtype;

          cleanup:
            H5Tclose(dtype);
            return status;
          } |]

getTextDatatype :: (HasCallStack, MonadResource m) => m Datatype
getTextDatatype =
  allocate (liftIO createTextDatatype) (liftIO . h5o_close) >>= \case
    (k, h) -> return $ Datatype (Handle h k)

-- withArrayViewStorable ::
--   forall a m b.
--   (Storable a, KnownDatatype a, MonadResource m) =>
--   a ->
--   (ArrayView -> m b) ->
--   m b
-- withArrayViewStorable x action = do
--   dtype <- ofType @a
--   dspace <- scalarDataspace
--   buffer <- liftIO . V.unsafeThaw . V.singleton $ x
--   r <- action (ArrayView dtype dspace (MV.unsafeCast buffer))
--   close dspace
--   close dtype
--   return r

-- peekArrayViewStorable ::
--   forall a m.
--   (HasCallStack, Storable a, KnownDatatype a, MonadResource m) =>
--   ArrayView ->
--   m a
-- peekArrayViewStorable (ArrayView dtype dspace buffer) = do
--   ofType @a >>= \object_dtype ->
--     checkDatatype object_dtype dtype >> close object_dtype
--   scalarDataspace >>= \object_dspace -> do
--     isSame <- h5s_extent_equal object_dspace dspace
--     unless isSame $
--       error "dataspace extents do not match; expected a scalar dataspace"
--     close object_dspace
--   liftIO $ MV.read (MV.unsafeCast buffer) 0

instance KnownDatatype Int32 where ofType = getStaticDatatype h5t_NATIVE_INT32

instance KnownDatatype Int64 where ofType = getStaticDatatype h5t_NATIVE_INT64

instance KnownDatatype Int where ofType = ofType @Int64

instance KnownDatatype Word32 where ofType = getStaticDatatype h5t_NATIVE_UINT32

instance KnownDatatype Word64 where ofType = getStaticDatatype h5t_NATIVE_UINT64

instance KnownDatatype Word where ofType = ofType @Word64

instance KnownDatatype CFloat where ofType = getStaticDatatype h5t_NATIVE_FLOAT

instance KnownDatatype Float where ofType = ofType @CFloat

instance KnownDatatype CDouble where ofType = getStaticDatatype h5t_NATIVE_DOUBLE

instance KnownDatatype Double where ofType = ofType @CDouble

instance KnownDatatype a => KnownDatatype (Complex a) where
  ofType = getComplexDatatype =<< ofType @a

instance KnownDatatype Text where ofType = getTextDatatype

instance KnownDatatype String where ofType = getTextDatatype

instance KnownDatatype B.ByteString where ofType = getTextDatatype

-- instance KnownDataset Int32 where
--   withArrayView = withArrayViewStorable
--   peekArrayView = peekArrayViewStorable

-- instance KnownDataset Int64 where
--   withArrayView = withArrayViewStorable
--   peekArrayView = peekArrayViewStorable

-- instance KnownDataset Int where
--   withArrayView = withArrayViewStorable
--   peekArrayView = peekArrayViewStorable

-- instance KnownDataset Word32 where
--   withArrayView = withArrayViewStorable
--   peekArrayView = peekArrayViewStorable

-- instance KnownDataset Word64 where
--   withArrayView = withArrayViewStorable
--   peekArrayView = peekArrayViewStorable

-- instance KnownDataset CFloat where
--   withArrayView = withArrayViewStorable
--   peekArrayView = peekArrayViewStorable

-- instance KnownDataset Float where
--   withArrayView = withArrayViewStorable
--   peekArrayView = peekArrayViewStorable

-- instance KnownDataset CDouble where
--   withArrayView = withArrayViewStorable
--   peekArrayView = peekArrayViewStorable

-- instance KnownDataset Double where
--   withArrayView = withArrayViewStorable
--   peekArrayView = peekArrayViewStorable

-- instance KnownDataset (Complex CFloat) where
--   withArrayView = withArrayViewStorable
--   peekArrayView = peekArrayViewStorable

-- instance KnownDataset (Complex Float) where
--   withArrayView = withArrayViewStorable
--   peekArrayView = peekArrayViewStorable

-- instance KnownDataset (Complex CDouble) where
--   withArrayView = withArrayViewStorable
--   peekArrayView = peekArrayViewStorable

-- instance KnownDataset (Complex Double) where
--   withArrayView = withArrayViewStorable
--   peekArrayView = peekArrayViewStorable

-- withArrayViewString :: (HasCallStack, MonadResource m) => ByteString -> (ArrayView -> m b) -> m b
-- withArrayViewString x action = do
--   dtype <- getTextDatatype
--   dspace <- scalarDataspace
--   buffer <- liftIO . V.unsafeThaw $ byteStringToVector x
--   r <- action (ArrayView dtype dspace buffer)
--   close dspace
--   close dtype
--   return r

-- peekArrayViewString :: (HasCallStack, MonadResource m) => ArrayView -> m ByteString
-- peekArrayViewString (ArrayView dtype dspace buffer) = do
--   getTextDatatype >>= \object_dtype ->
--     checkDatatype object_dtype dtype >> close object_dtype
--   scalarDataspace >>= \object_dspace -> do
--     isSame <- h5s_extent_equal object_dspace dspace
--     unless isSame $ error "dataspace extents do not match; expected a scalar dataspace"
--     close object_dspace
--   liftIO $ vectorToByteString <$> V.unsafeFreeze buffer

-- instance KnownDataset ByteString where
--   withArrayView = withArrayViewString
--   peekArrayView = peekArrayViewString

-- instance KnownDataset Text where
--   withArrayView x = withArrayViewString (encodeUtf8 x :: ByteString)
--   peekArrayView = (return . decodeUtf8) <=< peekArrayViewString

h5t_equal :: HasCallStack => Hid -> Hid -> IO Bool
h5t_equal a b = fromHtri =<< [CU.exp| htri_t { H5Tequal($(hid_t a), $(hid_t b)) } |]

fromHtri :: HasCallStack => Htri -> IO Bool
fromHtri x = (> 0) <$> checkError x

datatypeEqual :: HasCallStack => Datatype -> Datatype -> Bool
datatypeEqual a b = System.IO.Unsafe.unsafePerformIO $ h5t_equal (rawHandle a) (rawHandle b)

instance Eq Datatype where
  (==) = datatypeEqual

h5t_get_size :: Hid -> IO Int
h5t_get_size h =
  fmap fromIntegral . checkError
    =<< [C.block| int64_t {
          hsize_t size = H5Tget_size($(hid_t h));
          return size == 0 ? (-1) : (int64_t)size;
        } |]

datatypeSize :: Datatype -> Int
datatypeSize dtype = System.IO.Unsafe.unsafePerformIO (h5t_get_size (rawHandle dtype))

h5hl_dtype_to_text :: Datatype -> IO Text
h5hl_dtype_to_text dtype = do
  let h = rawHandle dtype
  numBytes <- c_dtype_to_text h nullPtr 0
  allocaBytes (numBytes + 1) $ \s -> c_dtype_to_text h s (numBytes + 1) >> fromCString s
 where
  c_dtype_to_text :: Hid -> Ptr CChar -> Int -> IO Int
  c_dtype_to_text x buffer size =
    let c_size = toUnsigned size
     in fmap fromIntegral . checkError
          =<< [CU.block| hssize_t {
              size_t len = $(size_t c_size);
              herr_t status = H5LTdtype_to_text($(hid_t x), $(char* buffer), H5LT_DDL, &len);
              return (status < 0) ? status : (hssize_t)len;
            } |]

datatypeName :: Datatype -> Text
datatypeName dtype = System.IO.Unsafe.unsafePerformIO (h5hl_dtype_to_text dtype)

instance GHC.Show.Show Datatype where
  show = toString . datatypeName

checkDatatype ::
  (HasCallStack, MonadIO m) =>
  -- | Expected
  Datatype ->
  -- | Encountered
  Datatype ->
  -- | Error when types do not match
  m ()
checkDatatype expected obtained =
  case datatypeEqual expected obtained of
    True -> pure ()
    False ->
      let nameExpected = datatypeName expected
          nameObtained = datatypeName obtained
       in error $
            "data type mismatch: expected " <> nameExpected <> ", but found " <> nameObtained

-- }}}
----------------------------------------------------------------------------------------------------
-- Dataspaces
----------------------------------------------------------------------------------------------------
-- {{{

h5s_close :: Hid -> IO ()
h5s_close h = do
  _ <- checkError =<< [CU.exp| herr_t { H5Sclose($(hid_t h)) } |]
  pure ()

createScalarDataspace :: IO Hid
createScalarDataspace = checkError =<< [CU.exp| hid_t { H5Screate(H5S_SCALAR) } |]

createSimpleDataspace :: [Int] -> IO Hid
createSimpleDataspace [] = createScalarDataspace
createSimpleDataspace sizes =
  withArrayLen (toUnsigned <$> sizes) $ \rank (c_sizes :: Ptr Hsize) ->
    let c_rank = fromIntegral rank
     in checkError
          =<< [CU.exp| hid_t { H5Screate_simple($(int c_rank), $(const hsize_t* c_sizes),
                                                $(const hsize_t* c_sizes)) } |]

simpleDataspace :: MonadResource m => [Int] -> m Dataspace
simpleDataspace sizes =
  allocate (liftIO $ createSimpleDataspace sizes) (liftIO . h5s_close) >>= \case
    (k, h) -> return $ Dataspace (Handle h k)

h5s_extent_equal :: (HasCallStack, MonadIO m) => Dataspace -> Dataspace -> m Bool
h5s_extent_equal dspace1 dspace2 = do
  let c_dspace1 = rawHandle dspace1
      c_dspace2 = rawHandle dspace2
  liftIO $
    fromHtri
      =<< [CU.exp| htri_t { H5Sextent_equal($(hid_t c_dspace1), $(hid_t c_dspace2)) } |]

h5s_get_simple_extent_ndims :: HasCallStack => Hid -> IO Int
h5s_get_simple_extent_ndims h =
  fmap fromIntegral . checkError
    =<< [C.exp| int { H5Sget_simple_extent_ndims($(hid_t h)) } |]

h5s_get_simple_extent_dims :: HasCallStack => Hid -> IO [Int]
h5s_get_simple_extent_dims h = do
  ndim <- h5s_get_simple_extent_ndims h
  allocaArray ndim $ \dimsPtr -> do
    _ <-
      checkError
        =<< [C.exp| int { H5Sget_simple_extent_dims($(hid_t h), $(hsize_t* dimsPtr), NULL) } |]
    fmap fromIntegral <$> peekArray ndim dimsPtr

dataspaceRank :: HasCallStack => Dataspace -> Int
dataspaceRank dataspace =
  withFrozenCallStack $
    System.IO.Unsafe.unsafePerformIO (h5s_get_simple_extent_ndims (rawHandle dataspace))

dataspaceShape :: HasCallStack => Dataspace -> [Int]
dataspaceShape dataspace = case dataspaceSelectionType dataspace of
  SelectedNone -> replicate (dataspaceRank dataspace) 0
  SelectedAll ->
    withFrozenCallStack $
      System.IO.Unsafe.unsafePerformIO (h5s_get_simple_extent_dims (rawHandle dataspace))
  _ ->
    if isRegularHyperslab dataspace
      then hyperslabShape . toHyperslab $ dataspace
      else error "cannot determine the size of not regularly-shaped selection"

data SelectionType = SelectedNone | SelectedPoints | SelectedHyperslabs | SelectedAll
  deriving (Read, Show, Eq)

dataspaceSelectionType :: HasCallStack => Dataspace -> SelectionType
dataspaceSelectionType dspace
  | toBool [CU.pure| bool { $(int c_sel_type) == H5S_SEL_NONE } |] = SelectedNone
  | toBool [CU.pure| bool { $(int c_sel_type) == H5S_SEL_POINTS } |] = SelectedPoints
  | toBool [CU.pure| bool { $(int c_sel_type) == H5S_SEL_HYPERSLABS } |] = SelectedHyperslabs
  | toBool [CU.pure| bool { $(int c_sel_type) == H5S_SEL_ALL } |] = SelectedAll
  | otherwise = error $ "invalid H5S_sel_type: " <> show c_sel_type
 where
  c_dspace = rawHandle dspace
  c_sel_type = [CU.pure| int { H5Sget_select_type($(hid_t c_dspace)) } |]

h5s_select_hyperslab :: HasCallStack => Hid -> [Int] -> [Int] -> [Int] -> IO ()
h5s_select_hyperslab h start size stride = do
  rank <- h5s_get_simple_extent_ndims h
  withArrayLen (toUnsigned <$> start) $ \startRank (c_start :: Ptr Hsize) ->
    withArrayLen (toUnsigned <$> size) $ \sizeRank (c_size :: Ptr Hsize) ->
      withArrayLen (toUnsigned <$> stride) $ \strideRank (c_stride :: Ptr Hsize) -> do
        unless (rank == startRank) . error $
          "'start' has wrong rank: " <> show startRank <> "; expected " <> show rank
        unless (rank == sizeRank) . error $
          "'size' has wrong rank: " <> show sizeRank <> "; expected " <> show rank
        unless (rank == strideRank) . error $
          "'stride' has wrong rank: " <> show strideRank <> "; expected " <> show rank
        _ <-
          checkError
            =<< [CU.exp| herr_t {
                  H5Sselect_hyperslab($(hid_t h), H5S_SELECT_SET,
                                      $(const hsize_t* c_start),
                                      $(const hsize_t* c_stride),
                                      $(const hsize_t* c_size),
                                      NULL)
                } |]
        isValid <- checkError =<< [CU.exp| hid_t { H5Sselect_valid($(hid_t h)) } |]
        when (isValid == 0) . error $
          "selection is invalid: start="
            <> show start
            <> ", size="
            <> show size
            <> ", stride="
            <> show stride
        return ()

h5s_is_regular_hyperslab :: HasCallStack => Hid -> IO Bool
h5s_is_regular_hyperslab h =
  fromHtri =<< [CU.exp| htri_t { H5Sis_regular_hyperslab($(hid_t h)) } |]

isRegularHyperslab :: HasCallStack => Dataspace -> Bool
isRegularHyperslab dspace =
  withFrozenCallStack $
    System.IO.Unsafe.unsafePerformIO (h5s_is_regular_hyperslab (rawHandle dspace))

-- compressHyperslabs :: (HasCallStack, MonadResource m) => Dataspace -> m Dataspace
-- compressHyperslabs dspace =
--   case isRegularHyperslab dspace of
--     True -> do
--       let c_dspace = rawHandle dspace
--           raw =
--             checkError
--               =<< [C.block| hid_t {
--                     hsize_t start[H5S_MAX_RANK];
--                     hsize_t stride[H5S_MAX_RANK];
--                     hsize_t count[H5S_MAX_RANK];
--                     hsize_t block[H5S_MAX_RANK];
--                     herr_t status =
--                       H5Sget_regular_hyperslab($(hid_t c_dspace), start, stride, count, block);
--                     if (status < 0) { return (hid_t)status; }
--                     int const rank = H5Sget_simple_extent_ndims($(hid_t c_dspace));
--                     if (rank < 0) { return (hid_t)rank; }
--                     hsize_t dims[H5S_MAX_RANK];
--                     for (int i = 0; i < rank; ++i) {
--                       dims[i] = count[i] * block[i];
--                     }
--                     return H5Screate_simple(rank, dims, dims);
--                   } |]
--       allocate (liftIO raw) (liftIO . h5s_close) >>= \case
--         (k, h) -> return $ Dataspace (Handle h k)
--     False -> error "only a single hyperslab can be compressed"

toUnsigned :: (HasCallStack, Show a, Integral a, Integral b) => a -> b
toUnsigned x
  | x < 0 = error $ "negative size or stride: " <> show x
  | otherwise = fromIntegral x

hyperslabRank :: Hyperslab -> Int
hyperslabRank = V.length . hyperslabStart

hyperslabShape :: Hyperslab -> [Int]
hyperslabShape h = V.toList $ V.zipWith (*) (hyperslabBlock h) (hyperslabCount h)

h5s_get_regular_hyperslab :: HasCallStack => Hid -> IO Hyperslab
h5s_get_regular_hyperslab h = do
  rank <- h5s_get_simple_extent_ndims h
  start <- MV.new rank
  stride <- MV.new rank
  count <- MV.new rank
  block <- MV.new rank
  !_ <- (checkError =<<) $
    MV.unsafeWith start $ \startPtr ->
      MV.unsafeWith stride $ \stridePtr ->
        MV.unsafeWith count $ \countPtr ->
          MV.unsafeWith block $ \blockPtr ->
            [CU.exp| herr_t {
              H5Sget_regular_hyperslab($(hid_t h), $(hsize_t* startPtr),
                $(hsize_t* stridePtr), $(hsize_t* countPtr), $(hsize_t* blockPtr)) } |]
  start' <- V.freeze start
  stride' <- V.freeze stride
  count' <- V.freeze count
  block' <- V.freeze block
  pure $
    Hyperslab
      (V.map fromIntegral start')
      (V.map fromIntegral stride')
      (V.map fromIntegral count')
      (V.map fromIntegral block')

toHyperslab :: HasCallStack => Dataspace -> Hyperslab
toHyperslab dspace =
  case dataspaceSelectionType dspace of
    SelectedNone -> error "nothing is selected"
    SelectedPoints -> error "points selection cannot be represented as a hyperslab"
    SelectedAll ->
      let shape = dataspaceShape dspace
          rank = dataspaceRank dspace
       in Hyperslab
            (V.replicate rank 0)
            (V.replicate rank 1)
            (fromList shape)
            (V.replicate rank 1)
    SelectedHyperslabs
      | isRegularHyperslab dspace ->
          System.IO.Unsafe.unsafePerformIO $
            withFrozenCallStack $
              h5s_get_regular_hyperslab (rawHandle dspace)
      | otherwise -> error "non-regular hyperslab cannot be converted to regular hyperslab"

checkSliceArguments :: HasCallStack => Hyperslab -> Int -> Int -> Int -> Int -> a -> a
checkSliceArguments hyperslab dim start count stride
  | dim < 0 || dim >= rank =
      error $ "invalid dim: " <> show dim <> "; dataspace is " <> show rank <> "-dimensional"
  | start < 0 || start >= extent =
      error $
        "invalid start: " <> show start <> "; dataspace shape is " <> show shape
  | stride < 1 = error $ "invalid stride: " <> show stride
  | count < 0 && count /= -1 = error $ "invalid count: " <> show count
  | V.any (/= 1) (hyperslabBlock hyperslab) =
      error $ "only size-1 blocks are supported"
  | otherwise = id
 where
  rank = hyperslabRank hyperslab
  extent = hyperslabCount hyperslab V.! dim
  shape = hyperslabCount hyperslab

sliceHyperslab :: HasCallStack => Int -> Int -> Int -> Int -> Hyperslab -> Hyperslab
sliceHyperslab dim start count stride hyperslab@(Hyperslab startV strideV countV blockV) =
  withFrozenCallStack $
    checkSliceArguments hyperslab dim start count stride $
      let newStart = start + (startV ! dim)
          newStride = (strideV ! dim) * stride
          newCount
            | count == -1 || start + count * stride > (countV ! dim) =
                (countV ! dim - start) `div` stride
            | otherwise = count
       in Hyperslab
            (startV V.// [(dim, newStart)])
            (strideV V.// [(dim, newStride)])
            (countV V.// [(dim, newCount)])
            blockV

selectHyperslab :: (HasCallStack, MonadResource m) => Hyperslab -> Dataspace -> m Dataspace
selectHyperslab hyperslab dspace = do
  let c_dspace = rawHandle dspace
      _with f = V.unsafeWith (V.map fromIntegral (f hyperslab))
      raw =
        (checkError =<<) $
          _with hyperslabStart $ \c_start ->
            _with hyperslabStride $ \c_stride ->
              _with hyperslabCount $ \c_count ->
                _with hyperslabBlock $ \c_block ->
                  [CU.block| hid_t {
                    hid_t new_dspace = H5Scopy($(hid_t c_dspace));
                    if (new_dspace < 0) { return new_dspace; }
                    herr_t status = H5Sselect_hyperslab(new_dspace, H5S_SELECT_SET,
                      $(const hsize_t* c_start), $(const hsize_t* c_stride),
                      $(const hsize_t* c_count), $(const hsize_t* c_block));
                    if (status < 0) {
                      H5Sclose(new_dspace);
                      return (hid_t)status;
                    }
                    return new_dspace;
                  } |]
  allocate (liftIO raw) (liftIO . h5s_close) >>= \case
    (k, h) -> return $ Dataspace (Handle h k)

sliceDataset :: HasCallStack => Int -> Int -> Int -> Int -> Dataset -> DatasetSlice
sliceDataset dim start count stride dataset =
  sliceDatasetSlice dim start count stride $ toSelection dataset

sliceDatasetSlice :: HasCallStack => Int -> Int -> Int -> Int -> DatasetSlice -> DatasetSlice
sliceDatasetSlice dim start count stride (DatasetSlice dataset hyperslab) =
  DatasetSlice dataset (sliceHyperslab dim start count stride hyperslab)

toSelection :: HasCallStack => Dataset -> DatasetSlice
toSelection dataset =
  System.IO.Unsafe.unsafePerformIO . runHDF5 $ do
    DatasetSlice dataset . toHyperslab <$> getDataspace dataset

sliceWithHyperslab :: HasCallStack => Hyperslab -> Dataset -> DatasetSlice
sliceWithHyperslab hyperslab dataset = DatasetSlice dataset hyperslab

class CanSlice t where
  slice :: Int -> Int -> Int -> Int -> t -> DatasetSlice

instance CanSlice Dataset where slice = sliceDataset

instance CanSlice DatasetSlice where slice = sliceDatasetSlice

{-
simpleStrides :: [Int] -> [Int]
simpleStrides = drop 1 . foldr acc [1]
  where
    acc x (y : ys) = let !r = x * y in (r : y : ys)
    acc _ _ = error "bug: this should never happen"
-}

{-
getMaxDims :: [Int] -> [Int] -> [Int]
getMaxDims dims strides
  | length dims == length strides = foldr acc [] $ zipWith (*) dims strides
  | otherwise = error "lengths of dims and strides do not match"
  where
    acc x (y : ys) = let !r = x `div` y in (r : y : ys)
    acc x [] = [x]
-}

-- }}}
----------------------------------------------------------------------------------------------------
-- Datassets
----------------------------------------------------------------------------------------------------
-- {{{

h5d_open :: HasCallStack => Hid -> Text -> IO Hid
h5d_open parent name =
  checkError
    =<< [C.exp| hid_t { H5Dopen($(hid_t parent), $bs-cstr:c_name, H5P_DEFAULT) } |]
 where
  c_name = encodeUtf8 name

createEmptyDataset ::
  (HasCallStack, MonadResource m) =>
  Group ->
  Text ->
  Datatype ->
  Dataspace ->
  m Dataset
createEmptyDataset parent name dtype dspace = do
  let c_parent = rawHandle parent
      c_name = encodeUtf8 name
      c_dtype = rawHandle dtype
      c_dspace = rawHandle dspace
      create =
        withFrozenCallStack $
          checkError
            =<< [C.exp| hid_t { H5Dcreate($(hid_t c_parent), $bs-cstr:c_name,
                                          $(hid_t c_dtype), $(hid_t c_dspace),
                                          H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) } |]
  (k, v) <- allocate (liftIO create) (liftIO . h5o_close)
  return $ Dataset (Handle v k)

h5d_create :: HasCallStack => Hid -> Text -> Datatype -> Dataspace -> IO Hid
h5d_create parent name dtype dspace =
  checkError
    =<< [C.exp| hid_t { H5Dcreate($(hid_t parent), $bs-cstr:c_name, $(hid_t c_dtype),
                                  $(hid_t c_dspace), H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) } |]
 where
  c_name = encodeUtf8 name
  c_dtype = rawHandle dtype
  c_dspace = rawHandle dspace

h5d_get_space :: HasCallStack => Dataset -> IO Hid
h5d_get_space dataset = checkError =<< [C.exp| hid_t { H5Dget_space($(hid_t h)) } |]
 where
  h = rawHandle dataset

getDataspace :: forall m. (HasCallStack, MonadResource m) => Dataset -> m Dataspace
getDataspace dataset = do
  (k, v) <- allocate (liftIO $ h5d_get_space dataset) (liftIO . h5s_close)
  return $ Dataspace (Handle v k)

getDatatype :: (HasCallStack, MonadResource m) => Dataset -> m Datatype
getDatatype dataset = do
  (k, v) <-
    allocate
      (liftIO $ withFrozenCallStack $ h5d_get_type (rawHandle dataset))
      (liftIO . h5o_close)
  return $ Datatype (Handle v k)

datasetDatatype :: (HasCallStack, MonadResource m) => Dataset -> m Datatype
datasetDatatype = getDatatype

h5d_get_type :: HasCallStack => Hid -> IO Hid
h5d_get_type h = checkError =<< [C.exp| hid_t { H5Dget_type($(hid_t h)) } |]

-- guessDataspace :: (HasCallStack, MonadResource m) => Dataspace -> m Dataspace
-- guessDataspace dspace = do
--   let c_dspace = rawHandle dspace
--   c_sel_type <-
--     liftIO $
--       checkError =<< [C.exp| int { H5Sget_select_type($(hid_t c_dspace)) } |]
--   if c_sel_type == [CU.pure| int { H5S_SEL_ALL } |]
--     then return dspace
--     else
--       if c_sel_type == [CU.pure| int { H5S_SEL_HYPERSLABS } |]
--         then compressHyperslabs dspace
--         else
--           error $
--             "dataspace can be automatically constructed only from "
--               <> "H5S_SEL_ALL or H5S_SEL_HYPERSLABS selections"

-- bufferSizeFor :: Datatype -> Dataspace -> Int
-- bufferSizeFor dtype dspace = product (dataspaceShape dspace) * datatypeSize dtype

isValidArrayView :: ArrayView' a -> Bool
isValidArrayView = const True

areStridesOkay :: [Int] -> Bool
areStridesOkay strides = go strides
 where
  go (x : y : ys) = if x `mod` y == 0 then go (y : ys) else False
  go _ = True

-- | Convert row-major strides (i.e. in decreasing order) into strides understood by HDF5.
prepareStrides :: [Int] -> [Int]
prepareStrides = snd . foldr combine (1, [])
 where
  combine x (c, ys) = let !y = x `div` c in (x, y : ys)

shouldExpand :: [Int] -> Bool
shouldExpand [] = False
shouldExpand strides = Data.List.last strides /= 1

arrayViewHyperslab :: ArrayView' a -> Hyperslab
arrayViewHyperslab (ArrayView' _ shape strides)
  | shouldExpand strides =
      Hyperslab
        (V.replicate (rank + 1) 0)
        (V.replicate (rank + 1) 1)
        (fromList (shape ++ [1]))
        (V.replicate (rank + 1) 1)
  | otherwise =
      Hyperslab
        (V.replicate rank 0)
        (V.replicate rank 1)
        (fromList shape)
        (V.replicate rank 1)
 where
  !rank = length strides

boundingBox :: ArrayView' a -> [Int]
boundingBox (ArrayView' _ _ []) = []
boundingBox (ArrayView' _ shape strides)
  | null strides = []
  | shouldExpand strides = box
  | otherwise = take rank box
 where
  rank = length strides
  box = Data.List.head shape : prepareStrides strides

-- dataspaceFromHyperslab :: MonadResource m => Hyperslab -> m Dataspace
-- dataspaceFromHyperslab hyperslab =
--   selectHyperslab hyperslab =<< simpleDataspace (V.toList box)
--   where
--     box =
--       V.zipWith3
--         (\start stride count -> start + stride * count)
--         (hyperslabStart hyperslab)
--         (hyperslabStride hyperslab)
--         (hyperslabCount hyperslab)

arrayViewDataspace :: (KnownDatatype a, MonadResource m) => ArrayView' a -> m Dataspace
arrayViewDataspace view@(ArrayView' _ _ []) = simpleDataspace []
arrayViewDataspace view@(ArrayView' _ _ strides)
  | areStridesOkay strides = selectHyperslab (arrayViewHyperslab view) =<< simpleDataspace (boundingBox view)
  -- r <-
  -- s1 <- dataspaceShape <$> simpleDataspace (boundingBox view)
  -- let s2 = dataspaceShape r
  -- trace (show (s1, s2, toHyperslab r)) $ pure ()
  -- pure r
  | otherwise = error $ "unsupported strides: " <> show strides

processSelection :: MonadResource m => DatasetSlice -> m Dataspace
processSelection (DatasetSlice dataset hyperslab) = do
  dataspace <- getDataspace dataset
  dataspace' <- selectHyperslab hyperslab dataspace
  close dataspace
  pure dataspace'

rowMajorStrides :: Integral a => [a] -> [a]
rowMajorStrides = drop 1 . scanr (*) 1

allocateForDataspace ::
  forall a m.
  (MonadResource m, KnownDatatype a) =>
  Dataspace ->
  m (ArrayView' a)
allocateForDataspace dataspace = do
  dtype <- ofType @a
  let hyperslab = toHyperslab dataspace
      elementSize = datatypeSize dtype
      shape = hyperslabShape hyperslab
      totalSize = elementSize * product shape
  ptr <- liftIO $ mallocForeignPtrBytes totalSize
  let !r = ArrayView' ptr shape (rowMajorStrides shape)
  close dtype
  pure r

newtype Scalar a = Scalar {unScalar :: a}
  deriving (Show, Eq)

type instance ElementOf (Scalar a) = a

instance (KnownDatatype a, Storable a) => KnownDataset' (Scalar a) where
  fromArrayView' (ArrayView' fp [] []) = Scalar <$> liftIO (withForeignPtr fp peek)
  fromArrayView' (ArrayView' _ shape _) = error $ "ArrayView has wrong shape: " <> show shape <> "; expected a rank-0 array"
  withArrayView' (Scalar x) action = do
    fp <- liftIO $ do
      buffer@(MVector _ fp) <- MV.new 1
      MV.write buffer 0 x
      pure fp
    action (ArrayView' fp [] [])

instance KnownDatatype a => KnownDataset' (ArrayView' a) where
  fromArrayView' = pure
  withArrayView' x action = action x

instance (KnownDatatype a, Storable a) => KnownDataset' (Vector a) where
  fromArrayView' (ArrayView' fp [size] [1]) = pure $ V.unsafeFromForeignPtr0 fp size
  fromArrayView' (ArrayView' fp [size] [stride])
    | size <= 0 = pure V.empty
    | otherwise = liftIO $ do
        buffer <- MV.new size
        withForeignPtr fp $ \ptr ->
          forM_ [0 .. size - 1] $ \i ->
            MV.write buffer i =<< peekElemOff ptr (i * stride)
        V.unsafeFreeze buffer
  fromArrayView' (ArrayView' _ shape strides) =
    error $
      "expected a 1-dimensional array, but shape and strides were "
        <> show shape
        <> " and "
        <> show strides
        <> " respectively"
  withArrayView' v action = action $ ArrayView' fp [n] [1]
   where
    (fp, n) = V.unsafeToForeignPtr0 v

_listFromArrayView1D ::
  (HasCallStack, KnownDatatype a, Storable a, MonadResource m) =>
  ArrayView' a ->
  m [a]
_listFromArrayView1D view = V.toList <$> fromArrayView' view

_listWithArrayView1D ::
  (HasCallStack, KnownDatatype a, Storable a, MonadResource m) =>
  [a] ->
  (ArrayView' a -> m b) ->
  m b
_listWithArrayView1D xs action = withArrayView' (V.fromList xs) action

_listFromArrayView2D ::
  forall a m.
  (HasCallStack, Storable a, MonadResource m) =>
  ArrayView' a ->
  m [[a]]
_listFromArrayView2D (ArrayView' fp [d₁, d₂] [s₁, s₂])
  | d₁ <= 0 = pure []
  | otherwise = liftIO . withForeignPtr fp $ \ptr -> loop₁ ptr (d₁ - 1) []
 where
  elemSize = let (_x :: a) = _x in sizeOf _x
  loop₂ ptr 0 acc = do !e <- peekElemOff ptr 0; pure (e : acc)
  loop₂ ptr j acc = do !e <- peekElemOff ptr (j * s₂); loop₂ ptr (j - 1) (e : acc)
  readRow ptr i
    | d₁ <= 0 = pure []
    | otherwise = loop₂ (ptr `plusPtr` (i * s₁ * elemSize)) (d₂ - 1) []
  loop₁ ptr 0 acc = do e <- readRow ptr 0; pure (e : acc)
  loop₁ ptr i acc = do e <- readRow ptr i; loop₁ ptr (i - 1) (e : acc)
_listFromArrayView2D (ArrayView' _ shape strides) =
  error $
    "expected a 2-dimensional array, but shape and strides were "
      <> show shape
      <> " and "
      <> show strides
      <> " respectively"

_listWithArrayView2D ::
  forall a m b.
  (HasCallStack, Storable a, MonadResource m) =>
  [[a]] ->
  (ArrayView' a -> m b) ->
  m b
_listWithArrayView2D xs action
  | d₁ == 0 = do
      fp <- liftIO $ newForeignPtr_ nullPtr
      action (ArrayView' fp [0, 0] [1, 1])
  | d₂ == 0 = do
      fp <- liftIO $ newForeignPtr_ nullPtr
      action (ArrayView' fp [d₁, 0] [1, 1])
  | otherwise = do
      unless (all ((== d₂) . length) (drop 1 xs)) $
        error $
          "nested list is not a matrix"
      fp <- liftIO $ mallocForeignPtrBytes (d₁ * d₂ * elemSize)
      liftIO . withForeignPtr fp $ \ptr -> pokeArray ptr (concat xs)
      action (ArrayView' fp [d₁, d₂] [d₂, 1])
 where
  d₁ = length xs
  d₂ = length (Data.List.head xs)
  elemSize = let (_x :: a) = _x in sizeOf _x

class (KnownNat d, KnownDatatype (ElementOf a)) => ListKnownDataset (d :: Nat) (a :: Type) where
  listFromArrayView :: MonadResource m => proxy d -> ArrayView' (ElementOf a) -> m a
  listWithArrayView :: MonadResource m => proxy d -> a -> (ArrayView' (ElementOf a) -> m b) -> m b

instance (ElementOf [a] ~ a, KnownDatatype a, Storable a) => ListKnownDataset 1 [a] where
  listFromArrayView _ = _listFromArrayView1D
  listWithArrayView _ = _listWithArrayView1D

instance (ElementOf [[a]] ~ a, KnownDatatype a, Storable a) => ListKnownDataset 2 [[a]] where
  listFromArrayView _ = _listFromArrayView2D
  listWithArrayView _ = _listWithArrayView2D

type family IsFinal a where
  IsFinal [a] = 'False
  IsFinal a = 'True

type family ListElement' a b where
  ListElement' a 'True = a
  ListElement' [a] 'False = ListElement' a (IsFinal a)

type ListElement (a :: Type) = ListElement' a (IsFinal a)

type family ListDimension' (n :: Nat) (a :: Type) where
  ListDimension' n [a] = ListDimension' (n + 1) a
  ListDimension' n a = n

type ListDimension (a :: Type) = ListDimension' 0 a

type instance ElementOf (ArrayView' a) = a

type instance ElementOf (Vector a) = a

type instance ElementOf [a] = ListElement [a]

instance ListKnownDataset (ListDimension [t]) [t] => KnownDataset' [t] where
  fromArrayView' = listFromArrayView (Proxy :: Proxy (ListDimension [t]))
  withArrayView' = listWithArrayView (Proxy :: Proxy (ListDimension [t]))

type instance ElementOf B.ByteString = B.ByteString

type instance ElementOf Text = Text

instance KnownDataset' B.ByteString where
  fromArrayView' (ArrayView' fp [] []) = do
    liftIO $
      withForeignPtr fp $
        (peek . castPtr) >=> B.packCString
  fromArrayView' (ArrayView' _ shape _) = error $ "ArrayView has wrong shape: " <> show shape <> "; expected a rank-0 array"
  withArrayView' (PS fp offset length) action = do
    c_string <- liftIO $ mallocForeignPtr
    liftIO $
      withForeignPtr fp $ \(p :: Ptr Word8) ->
        withForeignPtr c_string $ \c_string_ptr ->
          poke c_string_ptr (castPtr (p `plusPtr` offset) :: Ptr CChar)
    action (ArrayView' (castForeignPtr c_string) [] [])

-- withArrayViewString :: (HasCallStack, MonadResource m) => ByteString -> (ArrayView -> m b) -> m b
-- withArrayViewString x action = do
--   dtype <- getTextDatatype
--   dspace <- scalarDataspace
--   buffer <- liftIO . V.unsafeThaw $ byteStringToVector x
--   r <- action (ArrayView dtype dspace buffer)
--   close dspace
--   close dtype
--   return r

-- peekArrayViewString :: (HasCallStack, MonadResource m) => ArrayView -> m ByteString
-- peekArrayViewString (ArrayView dtype dspace buffer) = do
--   getTextDatatype >>= \object_dtype ->
--     checkDatatype object_dtype dtype >> close object_dtype
--   scalarDataspace >>= \object_dspace -> do
--     isSame <- h5s_extent_equal object_dspace dspace
--     unless isSame $ error "dataspace extents do not match; expected a scalar dataspace"
--     close object_dspace
--   liftIO $ vectorToByteString <$> V.unsafeFreeze buffer

-- instance KnownDataset ByteString where
--   withArrayView = withArrayViewString
--   peekArrayView = peekArrayViewString

unsafeCastArrayView :: ArrayView' a -> ArrayView' b
unsafeCastArrayView (ArrayView' fp shape stride) = ArrayView' (castForeignPtr fp) shape stride

instance KnownDataset' Text where
  fromArrayView' = (pure . (decodeUtf8 :: ByteString -> Text)) <=< (fromArrayView' . unsafeCastArrayView)
  withArrayView' x action = withArrayView' (encodeUtf8 x :: ByteString) (action . unsafeCastArrayView)

-- instance KnownDataset Text where
--   withArrayView x = withArrayViewString (encodeUtf8 x :: ByteString)
--   peekArrayView = (return . decodeUtf8) <=< peekArrayViewString

readDataset :: forall a m. (KnownDataset' a, MonadResource m) => Dataset -> m a
readDataset dataset = readDatasetImpl dataset =<< getDataspace dataset

readSelected :: forall a m. (KnownDataset' a, MonadResource m) => DatasetSlice -> m a
readSelected selection@(DatasetSlice dataset _) =
  readDatasetImpl dataset =<< processSelection selection

isVariableLengthString :: Datatype -> Bool
isVariableLengthString dtype =
  System.IO.Unsafe.unsafePerformIO $! runHDF5 $ (dtype ==) <$> ofType @Text
{-# NOINLINE isVariableLengthString #-}

-- H5T_class_t H5Tget_class(hid_t type_id)

cleanupStringAllocations :: MonadResource m => Datatype -> Dataspace -> ArrayView' a -> m ()
cleanupStringAllocations dtype dataspace (ArrayView' fp _ _)
  | isVariableLengthString dtype =
      liftIO . withForeignPtr fp $ \p -> do
        let type_id = rawHandle dtype
            space_id = rawHandle dataspace
            c_buf = castPtr p
        !_ <-
          checkError
            =<< [CU.exp| herr_t { H5Dvlen_reclaim($(hid_t type_id), $(hid_t space_id),
                                                    H5P_DEFAULT, $(void* c_buf)) } |]
        pure ()
  | otherwise = pure ()

readDatasetImpl :: forall a m. (KnownDataset' a, MonadResource m) => Dataset -> Dataspace -> m a
readDatasetImpl dataset dataspace = do
  view@(ArrayView' fp _ _) <- allocateForDataspace @(ElementOf a) dataspace
  readInplaceImpl view dataset dataspace
  !x <- fromArrayView' view
  -- Free variable-length strings which H5Dread might have allocated
  dtype <- datasetDatatype dataset
  cleanupStringAllocations dtype dataspace view
  close dtype
  pure x

readInplace :: (MonadResource m, KnownDataset' a) => a -> Dataset -> m ()
readInplace x dataset = readSelectedInplace x (toSelection dataset)

readSelectedInplace :: (MonadResource m, KnownDataset' a) => a -> DatasetSlice -> m ()
readSelectedInplace x selection@(DatasetSlice dataset _) = do
  dataspace <- processSelection selection
  withArrayView' x $ \view ->
    readInplaceImpl view dataset dataspace
  close dataspace

readInplaceImpl ::
  forall a m.
  (MonadResource m, KnownDatatype a) =>
  ArrayView' a ->
  Dataset ->
  Dataspace ->
  m ()
readInplaceImpl view@(ArrayView' fp shape stride) dataset dataspace = do
  fileDatatype <- getDatatype dataset
  memDatatype <- ofType @a
  unless (fileDatatype == memDatatype) $
    error $
      "datatype mismatch: you are trying to read "
        <> show memDatatype
        <> " from a dataset containing "
        <> show fileDatatype
  memDataspace <- arrayViewDataspace view
  let c_dataset = rawHandle dataset
      c_mem_type = rawHandle memDatatype
      c_mem_space = rawHandle memDataspace
      c_file_space = rawHandle dataspace
  !_ <-
    liftIO . withForeignPtr fp $ \ptr -> do
      let c_buf = castPtr ptr
      checkError
        =<< [C.exp| herr_t { H5Dread($(hid_t c_dataset), $(hid_t c_mem_type), $(hid_t c_mem_space),
                                   $(hid_t c_file_space), H5P_DEFAULT, $(void* c_buf)) } |]
  close fileDatatype
  close memDatatype
  close memDataspace

writeDatasetImpl ::
  forall a m.
  (HasCallStack, KnownDataset' a, MonadResource m) =>
  Dataset ->
  Dataspace ->
  a ->
  m ()
writeDatasetImpl dataset dataspace object = do
  fileDatatype <- getDatatype dataset
  memDatatype <- ofType @(ElementOf a)
  unless (fileDatatype == memDatatype) $
    error $
      "datatype mismatch: you are trying to write "
        <> show memDatatype
        <> " to a dataset containing "
        <> show fileDatatype
  withArrayView' object $ \view@(ArrayView' fp _ _) -> do
    memDataspace <- arrayViewDataspace view
    let c_dataset = rawHandle dataset
        c_mem_type = rawHandle memDatatype
        c_mem_space = rawHandle memDataspace
        c_file_space = rawHandle dataspace
    !_ <-
      liftIO . withForeignPtr fp $ \ptr -> do
        let c_buf = castPtr ptr
        checkError
          =<< [C.exp| herr_t { H5Dwrite($(hid_t c_dataset), $(hid_t c_mem_type),
                                        $(hid_t c_mem_space), $(hid_t c_file_space),
                                        H5P_DEFAULT, $(const void* c_buf)) } |]
    close memDataspace
  close fileDatatype
  close memDatatype

writeDataset ::
  forall a m.
  (HasCallStack, KnownDataset' a, MonadResource m) =>
  a ->
  Dataset ->
  m ()
writeDataset object dataset = do
  dataspace <- getDataspace dataset
  writeDatasetImpl dataset dataspace object
  close dataspace

writeSelected ::
  forall a m.
  (HasCallStack, KnownDataset' a, MonadResource m) =>
  a ->
  DatasetSlice ->
  m ()
writeSelected object selection@(DatasetSlice dataset _) = do
  dataspace <- processSelection selection
  writeDatasetImpl dataset dataspace object
  close dataspace

datasetRank :: HasCallStack => Dataset -> Int
datasetRank dataset =
  System.IO.Unsafe.unsafePerformIO . runHDF5 $
    return . dataspaceRank =<< getDataspace dataset

datasetShape :: HasCallStack => Dataset -> [Int]
datasetShape dataset =
  System.IO.Unsafe.unsafePerformIO . runHDF5 $
    return . dataspaceShape =<< getDataspace dataset

-- h5d_read :: forall a m. (HasCallStack, KnownDataset a, MonadResource m) => Dataset -> m a
-- h5d_read dataset = do
--   dtype <-
--     allocate (liftIO $ h5d_get_type dataset) (liftIO . h5o_close) >>= \case
--       (k, v) -> return $ Datatype (Handle v k)
--   dspace <-
--     allocate (liftIO $ h5d_get_space dataset) (liftIO . h5s_close) >>= \case
--       (k, v) -> return $ Dataspace (Handle v k)
--   sizeInBytes <- bufferSizeFor dtype dspace
--   buffer <- liftIO $ MV.unsafeNew sizeInBytes
--   let c_dataset = rawHandle dataset
--       c_dtype = rawHandle dtype
--       c_dspace = rawHandle dspace
--   !_ <- liftIO $
--     MV.unsafeWith buffer $ \bufferPtr ->
--       checkError
--         =<< [C.exp| herr_t { H5Dread($(hid_t c_dataset), $(hid_t c_dtype), $(hid_t c_dspace),
--                                    $(hid_t c_dspace), H5P_DEFAULT,
--                                    (void*)$(uint8_t* bufferPtr)) } |]
--   value <- peekArrayView $ ArrayView dtype dspace buffer
--   close dtype
--   close dspace
--   return value

-- h5d_write :: forall m. (HasCallStack, MonadResource m) => Dataset -> ArrayView -> m ()
-- h5d_write dataset (ArrayView object_dtype object_dspace buffer) = withFrozenCallStack $ do
--   dataset_dtype <- getDatatype dataset
--   checkDatatype object_dtype dataset_dtype
--   dataset_dspace <- getDataspace dataset
--   let c_dataset = rawHandle dataset
--       c_object_dtype = rawHandle object_dtype
--       c_object_dspace = rawHandle object_dspace
--       c_dataset_dspace = rawHandle dataset_dspace
--   !_ <- liftIO . MV.unsafeWith buffer $ \bufferPtr ->
--     checkError
--       =<< [C.exp| herr_t { H5Dwrite($(hid_t c_dataset), $(hid_t c_object_dtype), $(hid_t c_object_dspace),
--                                     $(hid_t c_dataset_dspace), H5P_DEFAULT,
--                                     (const void*)$(const uint8_t* bufferPtr)) } |]
--   return ()

-- instance (Storable a, KnownDatatype a) => KnownDataset [a] where
--   withArrayView xs = withArrayView (V.fromList xs)
--   peekArrayView view = V.toList <$> peekArrayView view

-- instance (Storable a, KnownDatatype a) => KnownDataset (Vector a) where
--   withArrayView v = withArrayView $ TemporaryContiguousArray [V.length v] v
--   peekArrayView view =
--     peekArrayView view
--       >>= \case
--         (TemporaryContiguousArray [n] v) -> assert (V.length v == n) $ return v
--         (TemporaryContiguousArray shape _) ->
--           error $
--             "array has wrong shape: " <> show shape <> "; expected a one-dimensional array"

data TemporaryContiguousArray a = TemporaryContiguousArray ![Int] !(Vector a)
  deriving stock (Eq, Show)

-- instance (Storable a, KnownDatatype a) => KnownDataset (TemporaryContiguousArray a) where
--   withArrayView (TemporaryContiguousArray shape v) action = do
--     when (any (< 0) shape) . error $
--       "invalid shape: " <> show shape
--     when (V.length v < product shape) . error $
--       "buffer is too short (" <> show (V.length v) <> " elements) for array of shape " <> show shape
--     dtype <- ofType @a
--     dspace <- simpleDataspace shape
--     buffer <- liftIO $ V.unsafeThaw v
--     r <- action . ArrayView dtype dspace . MV.unsafeCast $ buffer
--     close dspace
--     close dtype
--     return r
--   peekArrayView (ArrayView dtype dspace buffer) = do
--     ofType @a >>= \object_dtype ->
--       checkDatatype object_dtype dtype >> close object_dtype
--     dims <- liftIO $ h5s_get_simple_extent_dims dspace
--     sizeInBytes <- bufferSizeFor dtype dspace
--     unless (sizeInBytes == MV.length buffer) . error $
--       "bug: buffer has wrong size: "
--         <> show (MV.length buffer)
--         <> " bytes; expected "
--         <> show sizeInBytes
--         <> " for array of shape "
--         <> show dims
--     v <- liftIO . V.unsafeFreeze $ MV.unsafeCast buffer
--     return $ TemporaryContiguousArray dims v

data TemporaryStridedMatrix a = TemporaryStridedMatrix !(Int, Int) !Int !(Vector a)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

type instance ElementOf (TemporaryStridedMatrix a) = a

instance (Storable a, KnownDatatype a) => KnownDataset' (TemporaryStridedMatrix a) where
  withArrayView' (TemporaryStridedMatrix (d₀, d₁) s₀ v) action =
    action (ArrayView' (fst $ V.unsafeToForeignPtr0 v) [d₀, d₁] [s₀, 1])
  fromArrayView' (ArrayView' fp [d₀, d₁] [s₀, 1]) =
    pure $
      TemporaryStridedMatrix (d₀, d₁) s₀ (V.unsafeFromForeignPtr0 fp (d₀ * s₀))
  fromArrayView' (ArrayView' _ _ _) = error "failed to convert ArrayView' to TemporaryStridedMatrix; check your rank and strides"

-- instance (Storable a, KnownDatatype a) => KnownDataset (TemporaryStridedMatrix a) where
--   withArrayView (TemporaryStridedMatrix (d₀, d₁) s₀ v) action = do
--     when (d₀ < 0 || d₁ < 0 || s₀ < 0) . error $
--       "negative dimension or stride: " <> show (d₀, d₁) <> " " <> show s₀
--     when (s₀ < d₁) . error $
--       "invalid stride along the first dimension: " <> show s₀
--     when (V.length v < d₀ * s₀) . error $
--       "buffer is too short ("
--         <> show (V.length v)
--         <> " elements) for the bounding array of shape "
--         <> show (d₀, s₀)
--     dtype <- ofType @a
--     dspace <- simpleDataspace [d₀, s₀]
--     liftIO $ h5s_select_hyperslab dspace [0, 0] [d₀, d₁] [1, 1]
--     buffer <- liftIO $ V.unsafeThaw v
--     r <- action . ArrayView dtype dspace . MV.unsafeCast $ buffer
--     close dspace
--     close dtype
--     return r
--   peekArrayView view =
--     peekArrayView view >>= \case
--       (TemporaryContiguousArray [d₀, d₁] v) ->
--         assert (V.length v == d₀ * d₁) . return $
--           TemporaryStridedMatrix (d₀, d₁) d₁ v
--       (TemporaryContiguousArray shape _) ->
--         error $
--           "array has wrong shape: " <> show shape <> "; expected a matrix"

{-
getMaxDims :: [Int] -> [Int] -> [Int]
getMaxDims dims strides
  | length dims == length strides = foldr acc [] $ zipWith (*) dims strides
  | otherwise = error "lengths of dims and strides do not match"
  where
    acc x (y : ys) = let !r = x `div` y in (r : y : ys)
    acc x [] = [x]
-}

-- }}}
----------------------------------------------------------------------------------------------------
-- Attributes
----------------------------------------------------------------------------------------------------
-- {{{
h5a_close :: HasCallStack => Hid -> IO ()
h5a_close attr = void . checkError =<< [CU.exp| herr_t { H5Aclose($(hid_t attr)) } |]

openAttribute :: (HasCallStack, MonadResource m) => Object t -> Text -> m Attribute
openAttribute object name =
  allocate (liftIO acquire) (liftIO . h5a_close) >>= \case
    (k, v) -> return . Attribute $ Handle v k
 where
  c_object = rawHandle object
  c_name = encodeUtf8 name
  acquire = checkError =<< [CU.exp| hid_t { H5Aopen($(hid_t c_object), $bs-cstr:c_name, H5P_DEFAULT) } |]

h5a_create :: HasCallStack => Hid -> Text -> Hid -> Hid -> IO Hid
h5a_create object name dtype dspace =
  checkError
    =<< [C.exp| hid_t {
          H5Acreate($(hid_t object), $bs-cstr:c_name,
                    $(hid_t dtype), $(hid_t dspace),
                    H5P_DEFAULT, H5P_DEFAULT)
        } |]
 where
  c_name = encodeUtf8 name

createEmptyAttribute ::
  (HasCallStack, MonadResource m) =>
  Object t ->
  Text ->
  Datatype ->
  Dataspace ->
  m Attribute
createEmptyAttribute parent name dtype dspace =
  allocate acquire (liftIO . h5a_close) >>= \case
    (k, v) -> return . Attribute $ Handle v k
 where
  acquire =
    liftIO $
      withFrozenCallStack $
        h5a_create (rawHandle parent) name (rawHandle dtype) (rawHandle dspace)

attributeDatatype :: (HasCallStack, MonadResource m) => Attribute -> m Datatype
attributeDatatype attr =
  allocate
    (liftIO $ withFrozenCallStack $ h5a_get_type (rawHandle attr))
    (liftIO . h5o_close)
    >>= \case
      (k, v) -> return . Datatype $ Handle v k

h5a_get_type :: HasCallStack => Hid -> IO Hid
h5a_get_type h = checkError =<< [C.exp| hid_t { H5Aget_type($(hid_t h)) } |]

attributeDataspace :: (HasCallStack, MonadResource m) => Attribute -> m Dataspace
attributeDataspace attr =
  allocate
    (liftIO $ withFrozenCallStack $ h5a_get_space (rawHandle attr))
    (liftIO . h5s_close)
    >>= \case
      (k, v) -> return . Dataspace $ Handle v k

h5a_get_space :: HasCallStack => Hid -> IO Hid
h5a_get_space h = checkError =<< [C.exp| hid_t { H5Aget_space($(hid_t h)) } |]

-- h5a_read :: Hid -> Hid -> ArrayView' a -> IO ()
-- h5a_read attr dtype (ArrayView' fp _ _) = do

readAttribute' :: forall a m. (MonadResource m, KnownDataset' a) => Attribute -> m a
readAttribute' attr = do
  dtype <- attributeDatatype attr
  dspace <- attributeDataspace attr
  memDatatype <- ofType @(ElementOf a)
  view@(ArrayView' fp _ _) <- allocateForDataspace @(ElementOf a) dspace
  unless (dtype == memDatatype) $
    error $
      "datatype mismatch: you are trying to read "
        <> show memDatatype
        <> " from an attribute containing "
        <> show dtype
  !_ <-
    liftIO . withForeignPtr fp $ \ptr ->
      let c_attr = rawHandle attr
          c_dtype = rawHandle memDatatype
          c_buf = castPtr ptr
       in checkError
            =<< [C.exp| herr_t { H5Aread($(hid_t c_attr), $(hid_t c_dtype), $(void* c_buf)) } |]
  !x <- fromArrayView' view
  cleanupStringAllocations dtype dspace view
  close memDatatype
  close dspace
  close dtype
  pure x

readAttribute :: (KnownDataset' a, MonadResource m) => Object t -> Text -> m a
readAttribute object name = do
  attr <- openAttribute object name
  !x <- readAttribute' attr
  close attr
  pure x

writeAttribute :: forall a m t. (MonadResource m, KnownDataset' a) => Object t -> Text -> a -> m ()
writeAttribute object name x = do
  alreadyExists <- existsAttribute object name
  when alreadyExists $ deleteAttribute object name
  withArrayView' x $ \view@(ArrayView' fp _ _) -> do
    dtype <- ofType @(ElementOf a)
    dspace <- arrayViewDataspace view
    attr <- createEmptyAttribute object name dtype dspace
    !_ <- liftIO . withForeignPtr fp $ \ptr -> do
      let c_attr = rawHandle attr
          c_dtype = rawHandle dtype
          c_buf = castPtr ptr
       in checkError
            =<< [CU.exp| herr_t { H5Awrite($(hid_t c_attr), $(hid_t c_dtype),
                                           $(const void* c_buf)) } |]
    close attr
    close dspace
    close dtype

-- view@(ArrayView' fp _ _) <- allocateForDataspace @(ElementOf a) dspace
-- unless (dtype == memDatatype) $
--   error $
--     "datatype mismatch: you are trying to read "
--       <> show memDatatype
--       <> " from an attribute containing "
--       <> show dtype
-- !_ <-
--   liftIO . withForeignPtr fp $ \ptr ->
--     let c_attr = rawHandle attr
--         c_dtype = rawHandle memDatatype
--         c_buf = castPtr ptr
--      in checkError
--           =<< [C.exp| herr_t { H5Aread($(hid_t c_attr), $(hid_t c_dtype), $(void* c_buf)) } |]
-- !x <- fromArrayView' view
-- cleanupStringAllocations dtype dspace view
-- close memDatatype
-- close dspace
-- close dtype
-- pure x

-- h5a_read :: forall a m. (HasCallStack, KnownDataset a, MonadResource m) => Hid -> Text -> m a
-- h5a_read object name = do
--   attr <- h5a_open object name
--   dtype <- h5a_get_type attr
--   dspace <- h5a_get_space attr
--   buffer <- liftIO . MV.unsafeNew =<< bufferSizeFor dtype dspace
--   close dspace
--   let c_attr = rawHandle attr
--       c_dtype = rawHandle dtype
--   !_ <- liftIO $
--     MV.unsafeWith buffer $ \bufferPtr ->
--       checkError
--         =<< [C.exp| herr_t { H5Aread($(hid_t c_attr), $(hid_t c_dtype), (void*)$(uint8_t* bufferPtr)) } |]
--   value <- peekArrayView $ ArrayView dtype dspace buffer
--   close dtype
--   close attr
--   return value

-- h5a_write :: forall a m. (HasCallStack, KnownDataset a, MonadResource m) => Hid -> Text -> a -> m ()
-- h5a_write object name value =
--   withArrayView value $ \(ArrayView object_dtype object_dspace buffer) -> do
--     alreadyExists <- liftIO $ h5a_exists object name
--     unless alreadyExists $
--       h5a_create object name object_dtype object_dspace >>= close
--     attr <- h5a_open object name
--     h5a_get_type attr >>= \attr_dtype ->
--       checkDatatype object_dtype attr_dtype >> close attr_dtype
--     h5a_get_space attr >>= \attr_dspace -> do
--       isSame <- h5s_extent_equal object_dspace attr_dspace
--       unless isSame $ do
--         object_dims <- liftIO $ h5s_get_simple_extent_dims object_dspace
--         attr_dims <- liftIO $ h5s_get_simple_extent_dims attr_dspace
--         error $ "dataspace extents do not match: " <> show object_dims <> " != " <> show attr_dims
--       close attr_dspace
--     let c_attr = rawHandle attr
--         c_object_dtype = rawHandle object_dtype
--     _ <- liftIO $
--       MV.unsafeWith buffer $ \bufferPtr ->
--         checkError
--           =<< [C.exp| herr_t { H5Awrite($(hid_t c_attr), $(hid_t c_object_dtype),
--                                         (void const*)$(uint8_t const* bufferPtr)) } |]
--     close attr
--     return ()

h5a_exists :: HasCallStack => Hid -> Text -> IO Bool
h5a_exists object name = fromHtri =<< [C.exp| htri_t { H5Aexists($(hid_t object), $bs-cstr:c_name) } |]
 where
  c_name = encodeUtf8 name

existsAttribute :: (HasCallStack, MonadIO m) => Object t -> Text -> m Bool
existsAttribute object name = liftIO $ withFrozenCallStack $ h5a_exists (rawHandle object) name

h5a_delete :: HasCallStack => Hid -> Text -> IO ()
h5a_delete object name = void . checkError =<< [C.exp| herr_t { H5Adelete($(hid_t object), $bs-cstr:c_name) } |]
 where
  c_name = encodeUtf8 name

deleteAttribute :: (HasCallStack, MonadIO m) => Object t -> Text -> m ()
deleteAttribute object name = liftIO $ h5a_delete (rawHandle object) name

-- }}}
