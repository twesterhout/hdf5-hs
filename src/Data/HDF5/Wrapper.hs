{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.HDF5.Wrapper
  ( disableDiagOutput,

    -- * Files
    h5f_open,
    h5f_create,
    h5f_close,
    AccessFlags (..),
    h5f_get_filesize,

    -- * Objects
    h5o_close,
    h5o_open,
    h5o_open_by_idx,

    -- * Groups
    h5g_get_num_objs,
    h5g_create,

    -- * Links
    h5l_iterate,
    h5l_delete,
    h5l_exists,

    -- * Identifiers
    h5i_get_name,
    h5i_get_file_id,
    h5i_get_type,

    -- * Datatypes
    h5t_equal,
    h5t_get_size,
    h5hl_dtype_to_text,
    checkDatatype,

    -- * Dataspaces
    h5s_close,
    h5s_get_simple_extent_ndims,
    h5s_get_simple_extent_dims,
    h5s_select_hyperslab,
    simpleDataspace,
    scalarDataspace,
    guessDataspace,
    getHyperslab,
    sliceHyperslab,
    selectHyperslab,
    slice,
    dataspaceSelectionType,
    SelectionType (..),

    -- * Datasets
    h5d_open,
    h5d_create,
    h5d_get_space,
    h5d_get_type,
    h5d_read,
    h5d_write,
    TemporaryContiguousArray (..),
    TemporaryStridedMatrix (..),
    getDataspace,

    -- * Attributes
    h5a_open,
    h5a_create,
    h5a_read,
    h5a_write,
    h5a_exists,
    h5a_delete,

    -- * Helpers
    checkError,
    fromHtri,
    H5Exception (..),
  )
where

import Control.Exception.Safe
import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import Data.Complex
import Data.HDF5.Context
import Data.HDF5.Types
import qualified Data.List
import Data.Vector.Storable (Vector, (!))
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.ByteString
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.C.String (CString)
import Foreign.C.Types (CChar, CDouble, CFloat, CUInt)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (allocaArray, peekArray, withArrayLen)
import Foreign.Marshal.Utils (toBool)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr, nullPtr)
import Foreign.Storable
import qualified GHC.Show
import GHC.Stack
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import qualified System.IO.Unsafe
import Prelude hiding (first, group)

C.context (C.baseCtx <> C.bsCtx <> C.funCtx <> h5Ctx)
C.include "<hdf5.h>"
C.include "<hdf5_hl.h>"

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
    <$> (fromCString =<< [C.exp| const char* { $(H5E_error2_t* p)->file_name } |])
    <*> (fromCString =<< [C.exp| const char* { $(H5E_error2_t* p)->func_name } |])
    <*> (fromIntegral <$> [C.exp| unsigned int { $(H5E_error2_t* p)->line } |])
    <*> (fromCString =<< [C.exp| const char* { $(H5E_error2_t* p)->desc } |])

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
    h5e_get_current_stack = checkError' <$> [C.exp| hid_t { H5Eget_current_stack() } |]
    h5e_close_stack :: HasCallStack => Hid -> IO ()
    h5e_close_stack x = checkError' <$> [C.exp| herr_t { H5Eclose_stack($(hid_t x)) } |]
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

checkError :: (HasCallStack, Integral a) => a -> IO a
checkError = _checkError Nothing

-- }}}
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

-- | Open an existing HDF5 file.
h5f_open :: HasCallStack => Text -> AccessFlags -> IO Hid
h5f_open filename flags = do
  let c_filename = encodeUtf8 filename
      c_flags = accessFlagsToUInt flags
  checkError =<< [CU.exp| hid_t { H5Fopen($bs-cstr:c_filename, $(unsigned int c_flags), H5P_DEFAULT) } |]

-- | Create a new HDF5 file.
h5f_create :: HasCallStack => Text -> AccessFlags -> IO Hid
h5f_create filename flags = do
  let c_filename = encodeUtf8 filename
      c_flags = case flags of
        ReadOnly -> error "cannot create a file with ReadOnly access mode, use WriteTruncate instead"
        WriteAppend -> accessFlagsToUInt WriteTruncate
        WriteTruncate -> accessFlagsToUInt WriteTruncate
  checkError =<< [CU.exp| hid_t { H5Fcreate($bs-cstr:c_filename, $(unsigned int c_flags), H5P_DEFAULT, H5P_DEFAULT) } |]

-- | Close a file.
--
-- /Note:/ we need it because files cannot be closed using @H5Gclose@. See
-- <https://portal.hdfgroup.org/display/HDF5/H5O_CLOSE> for more info.
h5f_close :: HasCallStack => Hid -> IO ()
h5f_close file = void . checkError =<< [CU.exp| herr_t { H5Fclose($(hid_t file)) } |]

-- | Get size of a file.
h5f_get_filesize ::
  HasCallStack =>
  -- | File handle
  Hid ->
  -- | File size in bytes
  IO Int
h5f_get_filesize file =
  fmap fromIntegral . checkError
    =<< [C.block| int64_t {
          hsize_t size;
          herr_t status = H5Fget_filesize($(hid_t file), &size);
          return status < 0 ? (int64_t)status : (int64_t)size;
        } |]

-- }}}
----------------------------------------------------------------------------------------------------
-- Objects
----------------------------------------------------------------------------------------------------
-- {{{

-- | Close an object.
h5o_close :: HasCallStack => Hid -> IO ()
h5o_close h = void . checkError =<< [C.exp| herr_t { H5Oclose($(hid_t h)) } |]

-- | Open an object by name.
h5o_open ::
  HasCallStack =>
  -- | @parent@ file or group
  Hid ->
  -- | path to object relative to @parent@.
  Text ->
  -- | new object handle
  IO Hid
h5o_open parent path = checkError =<< [C.exp| hid_t { H5Oopen($(hid_t parent), $bs-cstr:c_path, H5P_DEFAULT) } |]
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
    checkError
      =<< [CU.exp| hid_t { H5Oopen_by_idx($(hid_t parent), ".", H5_INDEX_NAME,
                                          H5_ITER_INC, $(hsize_t c_index), H5P_DEFAULT) } |]
  where
    c_index = fromIntegral index

-- }}}
----------------------------------------------------------------------------------------------------
-- Groups
----------------------------------------------------------------------------------------------------
-- {{{

-- | Get number of objects in a group.
h5g_get_num_objs ::
  -- | file or group handle
  Hid ->
  -- | number of object
  IO Int
h5g_get_num_objs h =
  fmap fromIntegral . checkError
    =<< [CU.block| int64_t {
          H5G_info_t group_info;
          herr_t status = H5Gget_info($(hid_t h), &group_info);
          return status < 0 ? (int64_t)status : (int64_t)group_info.nlinks;
        } |]

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
          return H5Gclose(g); } |]
  return ()
  where
    c_path = encodeUtf8 path

-- }}}
----------------------------------------------------------------------------------------------------
-- Links
----------------------------------------------------------------------------------------------------
-- {{{
type H5L_iterate_t = Hid -> Ptr CChar -> Ptr H5L_info_t -> Ptr () -> IO Herr

foreign import ccall "wrapper"
  mkH5L_iterate_t :: H5L_iterate_t -> IO (FunPtr H5L_iterate_t)

-- | Iterate over immediate children of an object, think 'forM_' but for HDF5
-- groups.
h5l_iterate ::
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
            H5Literate($(hid_t group),
                       H5_INDEX_NAME,
                       H5_ITER_INC,
                       NULL,
                       $(herr_t (* actionPtr)(hid_t, const char*, const H5L_info_t*, void*)),
                       NULL)
          } |]

-- | Delete a link.
h5l_delete ::
  HasCallStack =>
  -- | parent file or group
  Hid ->
  -- | link path
  Text ->
  IO ()
h5l_delete parent path =
  void . checkError =<< [CU.exp| herr_t { H5Ldelete($(hid_t parent), $bs-cstr:c_path, H5P_DEFAULT) } |]
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
h5l_exists parent path = toBool <$> [CU.exp| htri_t { H5Lexists($(hid_t parent), $bs-cstr:c_path, H5P_DEFAULT) } |]
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
            =<< [C.exp| hssize_t { H5Iget_name($(hid_t x), $(char* buffer), $(size_t c_size)) } |]

-- | Get parent file.
h5i_get_file_id ::
  Hid ->
  -- | new file handle which must be released using 'h5f_close'
  IO Hid
h5i_get_file_id h = checkError =<< [C.exp| hid_t { H5Iget_file_id($(hid_t h)) } |]

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
      =<< [C.block| int {
            const H5I_type_t t = H5Iget_type($(hid_t h));
            return t == H5I_BADID ? -1 : t; } |]

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
      =<< [C.block| hid_t {
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
      =<< [C.block| hid_t {
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

withArrayViewStorable :: forall a m b. (Storable a, KnownDatatype a, MonadResource m) => a -> (ArrayView -> m b) -> m b
withArrayViewStorable x action = do
  dtype <- ofType @a
  dspace <- scalarDataspace
  buffer <- liftIO . V.unsafeThaw . V.singleton $ x
  r <- action (ArrayView dtype dspace (MV.unsafeCast buffer))
  close dspace
  close dtype
  return r

peekArrayViewStorable :: forall a m. (HasCallStack, Storable a, KnownDatatype a, MonadResource m) => ArrayView -> m a
peekArrayViewStorable (ArrayView dtype dspace buffer) = do
  ofType @a >>= \object_dtype ->
    checkDatatype object_dtype dtype >> close object_dtype
  scalarDataspace >>= \object_dspace -> do
    isSame <- h5s_extent_equal object_dspace dspace
    unless isSame $
      error "dataspace extents do not match; expected a scalar dataspace"
    close object_dspace
  liftIO $ MV.read (MV.unsafeCast buffer) 0

instance KnownDatatype Int32 where ofType = getStaticDatatype h5t_NATIVE_INT32

instance KnownDatatype Int64 where ofType = getStaticDatatype h5t_NATIVE_INT64

instance KnownDatatype Int where ofType = ofType @Int64

instance KnownDatatype Word32 where ofType = getStaticDatatype h5t_NATIVE_UINT32

instance KnownDatatype Word64 where ofType = getStaticDatatype h5t_NATIVE_UINT64

instance KnownDatatype CFloat where ofType = getStaticDatatype h5t_NATIVE_FLOAT

instance KnownDatatype Float where ofType = ofType @CFloat

instance KnownDatatype CDouble where ofType = getStaticDatatype h5t_NATIVE_DOUBLE

instance KnownDatatype Double where ofType = ofType @CDouble

instance KnownDatatype a => KnownDatatype (Complex a) where ofType = getComplexDatatype =<< ofType @a

instance KnownDatatype Text where ofType = getTextDatatype

instance KnownDatatype String where ofType = getTextDatatype

instance KnownDatatype ByteString where ofType = getTextDatatype

instance KnownDataset Int32 where
  withArrayView = withArrayViewStorable
  peekArrayView = peekArrayViewStorable

instance KnownDataset Int64 where
  withArrayView = withArrayViewStorable
  peekArrayView = peekArrayViewStorable

instance KnownDataset Int where
  withArrayView = withArrayViewStorable
  peekArrayView = peekArrayViewStorable

instance KnownDataset Word32 where
  withArrayView = withArrayViewStorable
  peekArrayView = peekArrayViewStorable

instance KnownDataset Word64 where
  withArrayView = withArrayViewStorable
  peekArrayView = peekArrayViewStorable

instance KnownDataset CFloat where
  withArrayView = withArrayViewStorable
  peekArrayView = peekArrayViewStorable

instance KnownDataset Float where
  withArrayView = withArrayViewStorable
  peekArrayView = peekArrayViewStorable

instance KnownDataset CDouble where
  withArrayView = withArrayViewStorable
  peekArrayView = peekArrayViewStorable

instance KnownDataset Double where
  withArrayView = withArrayViewStorable
  peekArrayView = peekArrayViewStorable

instance KnownDataset (Complex CFloat) where
  withArrayView = withArrayViewStorable
  peekArrayView = peekArrayViewStorable

instance KnownDataset (Complex Float) where
  withArrayView = withArrayViewStorable
  peekArrayView = peekArrayViewStorable

instance KnownDataset (Complex CDouble) where
  withArrayView = withArrayViewStorable
  peekArrayView = peekArrayViewStorable

instance KnownDataset (Complex Double) where
  withArrayView = withArrayViewStorable
  peekArrayView = peekArrayViewStorable

withArrayViewString :: (HasCallStack, MonadResource m) => ByteString -> (ArrayView -> m b) -> m b
withArrayViewString x action = do
  dtype <- getTextDatatype
  dspace <- scalarDataspace
  buffer <- liftIO . V.unsafeThaw $ byteStringToVector x
  r <- action (ArrayView dtype dspace buffer)
  close dspace
  close dtype
  return r

peekArrayViewString :: (HasCallStack, MonadResource m) => ArrayView -> m ByteString
peekArrayViewString (ArrayView dtype dspace buffer) = do
  getTextDatatype >>= \object_dtype ->
    checkDatatype object_dtype dtype >> close object_dtype
  scalarDataspace >>= \object_dspace -> do
    isSame <- h5s_extent_equal object_dspace dspace
    unless isSame $ error "dataspace extents do not match; expected a scalar dataspace"
    close object_dspace
  liftIO $ vectorToByteString <$> V.unsafeFreeze buffer

instance KnownDataset ByteString where
  withArrayView = withArrayViewString
  peekArrayView = peekArrayViewString

instance KnownDataset Text where
  withArrayView x = withArrayViewString (encodeUtf8 x :: ByteString)
  peekArrayView = (return . decodeUtf8) <=< peekArrayViewString

h5t_equal :: HasCallStack => Datatype -> Datatype -> IO Bool
h5t_equal dtype1 dtype2 = fromHtri =<< [C.exp| htri_t { H5Tequal($(hid_t h1), $(hid_t h2)) } |]
  where
    h1 = rawHandle dtype1
    h2 = rawHandle dtype2

fromHtri :: HasCallStack => Htri -> IO Bool
fromHtri x = (> 0) <$> checkError x

h5t_get_size :: Datatype -> IO Int
h5t_get_size dtype =
  fmap fromIntegral . checkError
    =<< [C.block| int64_t {
          hsize_t size = H5Tget_size($(hid_t h));
          return size == 0 ? (-1) : (int64_t)size;
        } |]
  where
    h = rawHandle dtype

h5hl_dtype_to_text :: Datatype -> IO Text
h5hl_dtype_to_text dtype = do
  let h = rawHandle dtype
  numBytes <- c_dtype_to_text h nullPtr 0
  allocaBytes (numBytes + 1) $ \s -> c_dtype_to_text h s (numBytes + 1) >> fromCString s
  where
    c_dtype_to_text :: Hid -> Ptr CChar -> Int -> IO Int
    c_dtype_to_text x buffer size =
      let c_size = fromIntegral size
       in fmap fromIntegral . checkError
            =<< [C.block| hssize_t {
              size_t len = $(size_t c_size);
              herr_t status = H5LTdtype_to_text($(hid_t x), $(char* buffer), H5LT_DDL, &len);
              return (status < 0) ? status : (hssize_t)len;
            } |]

checkDatatype ::
  (HasCallStack, MonadIO m) =>
  -- | Expected
  Datatype ->
  -- | Encountered
  Datatype ->
  -- | Error when types do not match
  m ()
checkDatatype expected obtained =
  liftIO $
    h5t_equal expected obtained >>= \case
      True -> return ()
      False -> do
        nameExpected <- h5hl_dtype_to_text expected
        nameObtained <- h5hl_dtype_to_text obtained
        error $
          "data type mismatch: expected " <> nameExpected <> ", but found " <> nameObtained

-- }}}
----------------------------------------------------------------------------------------------------
-- Dataspaces
----------------------------------------------------------------------------------------------------
-- {{{

h5s_close :: Hid -> IO ()
h5s_close h = void . checkError =<< [C.exp| herr_t { H5Sclose($(hid_t h)) } |]

createScalarDataspace :: IO Hid
createScalarDataspace = checkError =<< [C.exp| hid_t { H5Screate(H5S_SCALAR) } |]

createSimpleDataspace :: [Int] -> IO Hid
createSimpleDataspace sizes =
  withArrayLen (toUnsigned <$> sizes) $ \rank (c_sizes :: Ptr Hsize) ->
    let c_rank = fromIntegral rank
     in checkError
          =<< [C.exp| hid_t { H5Screate_simple($(int c_rank), $(const hsize_t* c_sizes),
                                                 $(const hsize_t* c_sizes)) } |]

scalarDataspace :: MonadResource m => m Dataspace
scalarDataspace =
  allocate (liftIO createScalarDataspace) (liftIO . h5s_close) >>= \case
    (k, h) -> return $ Dataspace (Handle h k)

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

h5s_get_simple_extent_ndims :: HasCallStack => Dataspace -> IO Int
h5s_get_simple_extent_ndims dspace =
  fmap fromIntegral . checkError
    =<< [C.exp| int { H5Sget_simple_extent_ndims($(hid_t h)) } |]
  where
    h = rawHandle dspace

dataspaceRank :: Dataspace -> Int
dataspaceRank = System.IO.Unsafe.unsafePerformIO . h5s_get_simple_extent_ndims

dataspaceShape :: Dataspace -> [Int]
dataspaceShape = System.IO.Unsafe.unsafePerformIO . h5s_get_simple_extent_dims

h5s_get_simple_extent_dims :: HasCallStack => Dataspace -> IO [Int]
h5s_get_simple_extent_dims dspace = do
  ndim <- h5s_get_simple_extent_ndims dspace
  allocaArray ndim $ \dimsPtr -> do
    let h = rawHandle dspace
    _ <-
      checkError
        =<< [C.exp| int { H5Sget_simple_extent_dims($(hid_t h), $(hsize_t* dimsPtr), NULL) } |]
    fmap fromIntegral <$> peekArray ndim dimsPtr

data SelectionType = SelectedNone | SelectedPoints | SelectedHyperslabs | SelectedAll
  deriving (Read, Show, Eq)

dataspaceSelectionType :: Dataspace -> SelectionType
dataspaceSelectionType dspace
  | toBool [CU.pure| bool { $(int c_sel_type) == H5S_SEL_NONE } |] = SelectedNone
  | toBool [CU.pure| bool { $(int c_sel_type) == H5S_SEL_POINTS } |] = SelectedPoints
  | toBool [CU.pure| bool { $(int c_sel_type) == H5S_SEL_HYPERSLABS } |] = SelectedHyperslabs
  | toBool [CU.pure| bool { $(int c_sel_type) == H5S_SEL_ALL } |] = SelectedAll
  | otherwise = error $ "invalid H5S_sel_type: " <> show c_sel_type
  where
    c_dspace = rawHandle dspace
    c_sel_type = [CU.pure| int { H5Sget_select_type($(hid_t c_dspace)) } |]

h5s_select_hyperslab :: HasCallStack => Dataspace -> [Int] -> [Int] -> [Int] -> IO ()
h5s_select_hyperslab dspace start size stride = do
  rank <- h5s_get_simple_extent_ndims dspace
  withArrayLen (toUnsigned <$> start) $ \startRank (c_start :: Ptr Hsize) ->
    withArrayLen (toUnsigned <$> size) $ \sizeRank (c_size :: Ptr Hsize) ->
      withArrayLen (toUnsigned <$> stride) $ \strideRank (c_stride :: Ptr Hsize) -> do
        unless (rank == startRank) . error $
          "'start' has wrong rank: " <> show startRank <> "; expected " <> show rank
        unless (rank == sizeRank) . error $
          "'size' has wrong rank: " <> show sizeRank <> "; expected " <> show rank
        unless (rank == strideRank) . error $
          "'stride' has wrong rank: " <> show strideRank <> "; expected " <> show rank
        let h = rawHandle dspace
        _ <-
          checkError
            =<< [C.exp| herr_t {
                  H5Sselect_hyperslab($(hid_t h), H5S_SELECT_SET,
                                      $(const hsize_t* c_start),
                                      $(const hsize_t* c_stride),
                                      $(const hsize_t* c_size),
                                      NULL)
                } |]
        isValid <- checkError =<< [C.exp| hid_t { H5Sselect_valid($(hid_t h)) } |]
        when (isValid == 0) . error $
          "selection is invalid: start="
            <> show start
            <> ", size="
            <> show size
            <> ", stride="
            <> show stride
        return ()

h5s_is_regular_hyperslab :: HasCallStack => Dataspace -> IO Bool
h5s_is_regular_hyperslab dspace =
  withFrozenCallStack $
    fromHtri =<< [C.exp| htri_t { H5Sis_regular_hyperslab($(hid_t h)) } |]
  where
    h = rawHandle dspace

isRegularHyperslab :: Dataspace -> Bool
isRegularHyperslab = System.IO.Unsafe.unsafePerformIO . h5s_is_regular_hyperslab

compressHyperslabs :: (HasCallStack, MonadResource m) => Dataspace -> m Dataspace
compressHyperslabs dspace =
  liftIO (h5s_is_regular_hyperslab dspace) >>= \case
    True -> do
      let c_dspace = rawHandle dspace
          raw =
            checkError
              =<< [C.block| hid_t {
                    hsize_t start[H5S_MAX_RANK];
                    hsize_t stride[H5S_MAX_RANK];
                    hsize_t count[H5S_MAX_RANK];
                    hsize_t block[H5S_MAX_RANK];
                    herr_t status =
                      H5Sget_regular_hyperslab($(hid_t c_dspace), start, stride, count, block);
                    if (status < 0) { return (hid_t)status; }
                    int const rank = H5Sget_simple_extent_ndims($(hid_t c_dspace));
                    if (rank < 0) { return (hid_t)rank; }
                    hsize_t dims[H5S_MAX_RANK];
                    for (int i = 0; i < rank; ++i) {
                      dims[i] = count[i] * block[i];
                    }
                    return H5Screate_simple(rank, dims, dims);
                  } |]
      allocate (liftIO raw) (liftIO . h5s_close) >>= \case
        (k, h) -> return $ Dataspace (Handle h k)
    False -> error "only a single hyperslab can be compressed"

toUnsigned :: (Show a, Integral a, Integral b) => a -> b
toUnsigned x
  | x < 0 = error $ "negative size or stride: " <> show x
  | otherwise = fromIntegral x

hyperslabRank :: Hyperslab -> Int
hyperslabRank = V.length . hyperslabStart

getHyperslab :: Dataspace -> Hyperslab
getHyperslab dspace =
  case dataspaceSelectionType dspace of
    SelectedNone -> error "nothing is selected"
    SelectedPoints -> error "points selection cannot be represented as a hyperslab"
    SelectedHyperslabs -> System.IO.Unsafe.unsafePerformIO $ do
      unless (isRegularHyperslab dspace) $
        error "non-regular hyperslab cannot be converted to regular hyperslab"
      start <- MV.new rank
      stride <- MV.new rank
      count <- MV.new rank
      block <- MV.new rank
      let c_dspace = rawHandle dspace
      _ <- (checkError =<<) $
        MV.unsafeWith start $ \startPtr ->
          MV.unsafeWith stride $ \stridePtr ->
            MV.unsafeWith count $ \countPtr ->
              MV.unsafeWith block $ \blockPtr ->
                [C.exp| herr_t {
                  H5Sget_regular_hyperslab($(hid_t c_dspace), $(hsize_t* startPtr),
                    $(hsize_t* stridePtr), $(hsize_t* countPtr), $(hsize_t* blockPtr)) } |]
      start' <- V.freeze start
      stride' <- V.freeze stride
      count' <- V.freeze count
      block' <- V.freeze block
      return $
        Hyperslab
          (V.map fromIntegral start')
          (V.map fromIntegral stride')
          (V.map fromIntegral count')
          (V.map fromIntegral block')
    SelectedAll ->
      Hyperslab
        (V.replicate rank 0)
        (V.replicate rank 1)
        (fromList (dataspaceShape dspace))
        (V.replicate rank 1)
  where
    rank = dataspaceRank dspace

-- Hyperslab <$> V.map fromIntegral (V.freeze start)

checkSliceArguments :: Hyperslab -> Int -> Int -> Int -> Int -> a -> a
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

sliceHyperslab :: Int -> Int -> Int -> Int -> Hyperslab -> Hyperslab
sliceHyperslab dim start count stride hyperslab@(Hyperslab startV strideV countV blockV) =
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

selectHyperslab :: MonadResource m => Hyperslab -> Dataspace -> m Dataspace
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

sliceDataset :: Int -> Int -> Int -> Int -> Dataset -> DatasetSlice
sliceDataset dim start count stride dataset =
  System.IO.Unsafe.unsafePerformIO $
    runResourceT $ do
      !r <-
        DatasetSlice dataset . sliceHyperslab dim start count stride . getHyperslab
          <$> getDataspace dataset
      pure r

sliceDatasetSlice :: Int -> Int -> Int -> Int -> DatasetSlice -> DatasetSlice
sliceDatasetSlice dim start count stride (DatasetSlice dataset hyperslab) =
  DatasetSlice dataset (sliceHyperslab dim start count stride hyperslab)

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

getDatatype :: forall m. (HasCallStack, MonadResource m) => Dataset -> m Datatype
getDatatype dataset = do
  (k, v) <- allocate (liftIO $ h5d_get_type dataset) (liftIO . h5o_close)
  return $ Datatype (Handle v k)

h5d_get_type :: HasCallStack => Dataset -> IO Hid
h5d_get_type dataset = checkError =<< [C.exp| hid_t { H5Dget_type($(hid_t h)) } |]
  where
    h = rawHandle dataset

guessDataspace :: (HasCallStack, MonadResource m) => Dataspace -> m Dataspace
guessDataspace dspace = do
  let c_dspace = rawHandle dspace
  c_sel_type <-
    liftIO $
      checkError =<< [C.exp| int { H5Sget_select_type($(hid_t c_dspace)) } |]
  if c_sel_type == [CU.pure| int { H5S_SEL_ALL } |]
    then return dspace
    else
      if c_sel_type == [CU.pure| int { H5S_SEL_HYPERSLABS } |]
        then compressHyperslabs dspace
        else
          error $
            "dataspace can be automatically constructed only from "
              <> "H5S_SEL_ALL or H5S_SEL_HYPERSLABS selections"

bufferSizeFor :: MonadIO m => Datatype -> Dataspace -> m Int
bufferSizeFor dtype dspace =
  liftIO $
    (*) <$> (product <$> h5s_get_simple_extent_dims dspace)
      <*> (h5t_get_size dtype)

h5d_read :: forall a m. (HasCallStack, KnownDataset a, MonadResource m) => Dataset -> m a
h5d_read dataset = do
  dtype <-
    allocate (liftIO $ h5d_get_type dataset) (liftIO . h5o_close) >>= \case
      (k, v) -> return $ Datatype (Handle v k)
  dspace <-
    allocate (liftIO $ h5d_get_space dataset) (liftIO . h5s_close) >>= \case
      (k, v) -> return $ Dataspace (Handle v k)
  sizeInBytes <- bufferSizeFor dtype dspace
  buffer <- liftIO $ MV.unsafeNew sizeInBytes
  let c_dataset = rawHandle dataset
      c_dtype = rawHandle dtype
      c_dspace = rawHandle dspace
  !_ <- liftIO $
    MV.unsafeWith buffer $ \bufferPtr ->
      checkError
        =<< [C.exp| herr_t { H5Dread($(hid_t c_dataset), $(hid_t c_dtype), $(hid_t c_dspace),
                                   $(hid_t c_dspace), H5P_DEFAULT,
                                   (void*)$(uint8_t* bufferPtr)) } |]
  value <- peekArrayView $ ArrayView dtype dspace buffer
  close dtype
  close dspace
  return value

h5d_write :: forall m. (HasCallStack, MonadResource m) => Dataset -> ArrayView -> m ()
h5d_write dataset (ArrayView object_dtype object_dspace buffer) = withFrozenCallStack $ do
  dataset_dtype <- getDatatype dataset
  checkDatatype object_dtype dataset_dtype
  dataset_dspace <- getDataspace dataset
  let c_dataset = rawHandle dataset
      c_object_dtype = rawHandle object_dtype
      c_object_dspace = rawHandle object_dspace
      c_dataset_dspace = rawHandle dataset_dspace
  !_ <- liftIO . MV.unsafeWith buffer $ \bufferPtr ->
    checkError
      =<< [C.exp| herr_t { H5Dwrite($(hid_t c_dataset), $(hid_t c_object_dtype), $(hid_t c_object_dspace),
                                    $(hid_t c_dataset_dspace), H5P_DEFAULT,
                                    (const void*)$(const uint8_t* bufferPtr)) } |]
  return ()

instance (Storable a, KnownDatatype a) => KnownDataset [a] where
  withArrayView xs = withArrayView (V.fromList xs)
  peekArrayView view = V.toList <$> peekArrayView view

instance (Storable a, KnownDatatype a) => KnownDataset (Vector a) where
  withArrayView v = withArrayView $ TemporaryContiguousArray [V.length v] v
  peekArrayView view =
    peekArrayView view
      >>= \case
        (TemporaryContiguousArray [n] v) -> assert (V.length v == n) $ return v
        (TemporaryContiguousArray shape _) ->
          error $
            "array has wrong shape: " <> show shape <> "; expected a one-dimensional array"

data TemporaryContiguousArray a = TemporaryContiguousArray ![Int] !(Vector a)
  deriving stock (Eq, Show)

instance (Storable a, KnownDatatype a) => KnownDataset (TemporaryContiguousArray a) where
  withArrayView (TemporaryContiguousArray shape v) action = do
    when (any (< 0) shape) . error $
      "invalid shape: " <> show shape
    when (V.length v < product shape) . error $
      "buffer is too short (" <> show (V.length v) <> " elements) for array of shape " <> show shape
    dtype <- ofType @a
    dspace <- simpleDataspace shape
    buffer <- liftIO $ V.unsafeThaw v
    r <- action . ArrayView dtype dspace . MV.unsafeCast $ buffer
    close dspace
    close dtype
    return r
  peekArrayView (ArrayView dtype dspace buffer) = do
    ofType @a >>= \object_dtype ->
      checkDatatype object_dtype dtype >> close object_dtype
    dims <- liftIO $ h5s_get_simple_extent_dims dspace
    sizeInBytes <- bufferSizeFor dtype dspace
    unless (sizeInBytes == MV.length buffer) . error $
      "bug: buffer has wrong size: "
        <> show (MV.length buffer)
        <> " bytes; expected "
        <> show sizeInBytes
        <> " for array of shape "
        <> show dims
    v <- liftIO . V.unsafeFreeze $ MV.unsafeCast buffer
    return $ TemporaryContiguousArray dims v

data TemporaryStridedMatrix a = TemporaryStridedMatrix !(Int, Int) !Int !(Vector a)
  deriving stock (Eq, Show)

instance (Storable a, KnownDatatype a) => KnownDataset (TemporaryStridedMatrix a) where
  withArrayView (TemporaryStridedMatrix (d₀, d₁) s₀ v) action = do
    when (d₀ < 0 || d₁ < 0 || s₀ < 0) . error $
      "negative dimension or stride: " <> show (d₀, d₁) <> " " <> show s₀
    when (s₀ < d₁) . error $
      "invalid stride along the first dimension: " <> show s₀
    when (V.length v < d₀ * s₀) . error $
      "buffer is too short ("
        <> show (V.length v)
        <> " elements) for the bounding array of shape "
        <> show (d₀, s₀)
    dtype <- ofType @a
    dspace <- simpleDataspace [d₀, s₀]
    liftIO $ h5s_select_hyperslab dspace [0, 0] [d₀, d₁] [1, 1]
    buffer <- liftIO $ V.unsafeThaw v
    r <- action . ArrayView dtype dspace . MV.unsafeCast $ buffer
    close dspace
    close dtype
    return r
  peekArrayView view =
    peekArrayView view >>= \case
      (TemporaryContiguousArray [d₀, d₁] v) ->
        assert (V.length v == d₀ * d₁) . return $
          TemporaryStridedMatrix (d₀, d₁) d₁ v
      (TemporaryContiguousArray shape _) ->
        error $
          "array has wrong shape: " <> show shape <> "; expected a matrix"

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
h5a_close attr = void . checkError =<< [C.exp| herr_t { H5Aclose($(hid_t attr)) } |]

h5a_open :: (HasCallStack, MonadResource m) => Hid -> Text -> m Attribute
h5a_open object name =
  allocate (liftIO acquire) (liftIO . h5a_close) >>= \case
    (k, v) -> return . Attribute $ Handle v k
  where
    c_name = encodeUtf8 name
    acquire = checkError =<< [C.exp| hid_t { H5Aopen($(hid_t object), $bs-cstr:c_name, H5P_DEFAULT) } |]

h5a_create :: (HasCallStack, MonadResource m) => Hid -> Text -> Datatype -> Dataspace -> m Attribute
h5a_create object name dtype dspace = do
  let c_name = encodeUtf8 name
      c_dtype = rawHandle dtype
      c_dspace = rawHandle dspace
      acquire =
        checkError
          =<< [C.exp| hid_t {
                H5Acreate($(hid_t object), $bs-cstr:c_name,
                          $(hid_t c_dtype), $(hid_t c_dspace),
                          H5P_DEFAULT, H5P_DEFAULT)
              } |]
  allocate (liftIO acquire) (liftIO . h5a_close) >>= \case
    (k, v) -> return . Attribute $ Handle v k

h5a_get_type :: (HasCallStack, MonadResource m) => Attribute -> m Datatype
h5a_get_type attr =
  allocate (liftIO acquire) (liftIO . h5o_close) >>= \case
    (k, v) -> return . Datatype $ Handle v k
  where
    acquire = checkError =<< [C.exp| hid_t { H5Aget_type($(hid_t h)) } |]
    h = rawHandle attr

h5a_get_space :: (HasCallStack, MonadResource m) => Attribute -> m Dataspace
h5a_get_space attr =
  allocate (liftIO acquire) (liftIO . h5s_close) >>= \case
    (k, v) -> return . Dataspace $ Handle v k
  where
    acquire = checkError =<< [C.exp| hid_t { H5Aget_space($(hid_t h)) } |]
    h = rawHandle attr

h5a_read :: forall a m. (HasCallStack, KnownDataset a, MonadResource m) => Hid -> Text -> m a
h5a_read object name = do
  attr <- h5a_open object name
  dtype <- h5a_get_type attr
  dspace <- h5a_get_space attr
  buffer <- liftIO . MV.unsafeNew =<< bufferSizeFor dtype dspace
  close dspace
  let c_attr = rawHandle attr
      c_dtype = rawHandle dtype
  !_ <- liftIO $
    MV.unsafeWith buffer $ \bufferPtr ->
      checkError
        =<< [C.exp| herr_t { H5Aread($(hid_t c_attr), $(hid_t c_dtype), (void*)$(uint8_t* bufferPtr)) } |]
  value <- peekArrayView $ ArrayView dtype dspace buffer
  close dtype
  close attr
  return value

h5a_write :: forall a m. (HasCallStack, KnownDataset a, MonadResource m) => Hid -> Text -> a -> m ()
h5a_write object name value =
  withArrayView value $ \(ArrayView object_dtype object_dspace buffer) -> do
    alreadyExists <- liftIO $ h5a_exists object name
    unless alreadyExists $
      h5a_create object name object_dtype object_dspace >>= close
    attr <- h5a_open object name
    h5a_get_type attr >>= \attr_dtype ->
      checkDatatype object_dtype attr_dtype >> close attr_dtype
    h5a_get_space attr >>= \attr_dspace -> do
      isSame <- h5s_extent_equal object_dspace attr_dspace
      unless isSame $ do
        object_dims <- liftIO $ h5s_get_simple_extent_dims object_dspace
        attr_dims <- liftIO $ h5s_get_simple_extent_dims attr_dspace
        error $ "dataspace extents do not match: " <> show object_dims <> " != " <> show attr_dims
      close attr_dspace
    let c_attr = rawHandle attr
        c_object_dtype = rawHandle object_dtype
    _ <- liftIO $
      MV.unsafeWith buffer $ \bufferPtr ->
        checkError
          =<< [C.exp| herr_t { H5Awrite($(hid_t c_attr), $(hid_t c_object_dtype),
                                        (void const*)$(uint8_t const* bufferPtr)) } |]
    close attr
    return ()

h5a_exists :: HasCallStack => Hid -> Text -> IO Bool
h5a_exists object name = fromHtri =<< [C.exp| htri_t { H5Aexists($(hid_t object), $bs-cstr:c_name) } |]
  where
    c_name = encodeUtf8 name

h5a_delete :: HasCallStack => Hid -> Text -> IO ()
h5a_delete object name = void . checkError =<< [C.exp| herr_t { H5Adelete($(hid_t object), $bs-cstr:c_name) } |]
  where
    c_name = encodeUtf8 name

-- }}}
