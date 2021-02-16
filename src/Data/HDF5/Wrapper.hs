{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.HDF5.Wrapper
  ( -- * Files
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
    withScalarDataspace,
    withSimpleDataspace,
    withStridedDataspace,

    -- * Datasets
    h5d_open,
    h5d_create,
    h5d_get_space,
    h5d_get_type,
    h5d_read,
    h5d_write,

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
  )
where

import Control.Exception.Safe
import qualified Data.ByteString as B
import Data.Complex
import Data.Constraint
import Data.HDF5.Context
import Data.HDF5.Types
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.C.String (CString)
import Foreign.C.Types (CChar, CUInt)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (allocaArray, peekArray, withArrayLen)
import Foreign.Marshal.Utils (toBool, with)
import Foreign.Ptr (FunPtr, Ptr, castPtr, freeHaskellFunPtr, nullPtr)
import Foreign.Storable
import qualified GHC.Show
import GHC.Stack
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
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
                   H5E_WALK_UPWARD,
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

_checkError :: (HasCallStack, Integral a) => Maybe Text -> a -> IO a
_checkError msg !x
  | x < 0 = collectStack >>= \stack -> throw $ H5Exception (fromIntegral x) stack msg
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
  checkError =<< [CU.exp| hid_t { H5Fopen($bs-ptr:c_filename, $(unsigned int c_flags), H5P_DEFAULT) } |]

-- | Create a new HDF5 file.
h5f_create :: HasCallStack => Text -> AccessFlags -> IO Hid
h5f_create filename flags = do
  let c_filename = encodeUtf8 filename
      c_flags = accessFlagsToUInt flags
  checkError =<< [CU.exp| hid_t { H5Fcreate($bs-ptr:c_filename, $(unsigned int c_flags), H5P_DEFAULT, H5P_DEFAULT) } |]

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
h5o_open parent path = checkError =<< [C.exp| hid_t { H5Oopen($(hid_t parent), $bs-ptr:c_path, H5P_DEFAULT) } |]
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
          const hid_t g = H5Gcreate($(hid_t parent), $bs-ptr:c_path,
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
  -- | parent file or group
  Hid ->
  -- | link path
  Text ->
  IO ()
h5l_delete parent path =
  void . checkError =<< [CU.exp| herr_t { H5Ldelete($(hid_t parent), $bs-ptr:c_path, H5P_DEFAULT) } |]
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
h5l_exists parent path = toBool <$> [CU.exp| htri_t { H5Lexists($(hid_t parent), $bs-ptr:c_path, H5P_DEFAULT) } |]
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

withStaticDatatype :: Hid -> (Datatype -> m b) -> m b
withStaticDatatype p f = f (Datatype p)

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

createComplexDatatype :: Hid -> IO Hid
createComplexDatatype dtype =
  checkError
    =<< [C.block| hid_t {
      const size_t size = H5Tget_size($(hid_t dtype));
      if (size == 0) { return -1; }
      hid_t complex_dtype = H5Tcreate(H5T_COMPOUND, size);
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

withComplexDatatype :: (MonadIO m, MonadMask m) => Hid -> (Datatype -> m a) -> m a
withComplexDatatype dtype action =
  bracket (liftIO $ createComplexDatatype dtype) (liftIO . h5o_close) $ \t ->
    action (Datatype t)

createTextDatatype :: IO Hid
createTextDatatype =
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

withTextDatatype :: (MonadIO m, MonadMask m) => (Datatype -> m a) -> m a
withTextDatatype action =
  bracket (liftIO createTextDatatype) (liftIO . h5o_close) $ \t ->
    action (Datatype t)

instance KnownDatatype Int where
  withDatatype _ = withStaticDatatype h5t_NATIVE_INT64
  hasStorable _ = Just Dict

instance KnownDatatype Int32 where
  withDatatype _ = withStaticDatatype h5t_NATIVE_INT32
  hasStorable _ = Just Dict

instance KnownDatatype Int64 where
  withDatatype _ = withStaticDatatype h5t_NATIVE_INT64
  hasStorable _ = Just Dict

instance KnownDatatype Word32 where
  withDatatype _ = withStaticDatatype h5t_NATIVE_UINT32
  hasStorable _ = Just Dict

instance KnownDatatype Word64 where
  withDatatype _ = withStaticDatatype h5t_NATIVE_UINT64
  hasStorable _ = Just Dict

instance KnownDatatype Float where
  withDatatype _ = withStaticDatatype h5t_NATIVE_FLOAT
  hasStorable _ = Just Dict

instance KnownDatatype Double where
  withDatatype _ = withStaticDatatype h5t_NATIVE_DOUBLE
  hasStorable _ = Just Dict

instance KnownDatatype (Complex Float) where
  withDatatype _ = withComplexDatatype h5t_NATIVE_FLOAT
  hasStorable _ = Just Dict

instance KnownDatatype (Complex Double) where
  withDatatype _ = withComplexDatatype h5t_NATIVE_DOUBLE
  hasStorable _ = Just Dict

instance KnownDatatype ByteString where
  withDatatype _ = withTextDatatype
  h5Peek p n
    | n == pointerSize = peek (castPtr p :: Ptr (Ptr CChar)) >>= B.packCString
    | otherwise =
      error $
        "expected a buffer of length " <> show pointerSize <> " for variable-length string"
    where
      pointerSize = sizeOf (nullPtr :: Ptr CChar)
  h5With x func = B.useAsCString x $ \p -> with p $ \p' -> func (castPtr p') (sizeOf p)

instance KnownDatatype Text where
  withDatatype _ = withTextDatatype
  h5Peek p n = (decodeUtf8 :: ByteString -> Text) <$> h5Peek p n
  h5With x = h5With (encodeUtf8 x :: ByteString)

h5t_equal :: HasCallStack => Hid -> Hid -> IO Bool
h5t_equal dtype1 dtype2 = fromHtri =<< [C.exp| htri_t { H5Tequal($(hid_t dtype1), $(hid_t dtype2)) } |]

fromHtri :: HasCallStack => Htri -> IO Bool
fromHtri x = (> 0) <$> checkError x

h5t_get_size :: Hid -> IO Int
h5t_get_size h =
  fmap fromIntegral . checkError
    =<< [C.block| int64_t {
    hsize_t size = H5Tget_size($(hid_t h));
    return size == 0 ? (-1) : (int64_t)size;
  } |]

h5hl_dtype_to_text :: Hid -> IO Text
h5hl_dtype_to_text h = do
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
  HasCallStack =>
  -- | Expected
  Datatype ->
  -- | Encountered
  Datatype ->
  -- | Error when types do not match
  IO ()
checkDatatype (Datatype expected) (Datatype obtained) = do
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

withScalarDataspace :: (Dataspace -> IO a) -> IO a
withScalarDataspace action =
  bracket (checkError =<< [C.exp| hid_t { H5Screate(H5S_SCALAR) } |]) h5s_close $ \dspace ->
    action (Dataspace dspace)

withSimpleDataspace :: [Int] -> (Dataspace -> IO a) -> IO a
withSimpleDataspace sizes action =
  withArrayLen (toUnsigned <$> sizes) $ \rank (c_sizes :: Ptr Hsize) -> do
    let c_rank = fromIntegral rank
        acquire =
          checkError
            =<< [C.exp| hid_t { H5Screate_simple($(int c_rank), $(const hsize_t* c_sizes),
                                                 $(const hsize_t* c_sizes)) } |]
    bracket acquire h5s_close $ \dspace ->
      action (Dataspace dspace)

withStridedDataspace :: [Int] -> [Int] -> (Dataspace -> IO a) -> IO a
withStridedDataspace sizes strides action =
  withSimpleDataspace (getMaxDims sizes strides) $ \(Dataspace dspace) -> do
    h5s_select_hyperslab dspace (const 0 <$> sizes) sizes strides
    action (Dataspace dspace)

h5s_get_simple_extent_ndims :: HasCallStack => Hid -> IO Int
h5s_get_simple_extent_ndims dspace =
  fmap fromIntegral . checkError
    =<< [C.exp| int { H5Sget_simple_extent_ndims($(hid_t dspace)) } |]

h5s_get_simple_extent_dims :: HasCallStack => Hid -> IO [Int]
h5s_get_simple_extent_dims dspace = do
  ndim <- h5s_get_simple_extent_ndims dspace
  allocaArray ndim $ \dimsPtr -> do
    _ <-
      checkError
        =<< [C.exp| int { H5Sget_simple_extent_dims($(hid_t dspace), $(hsize_t* dimsPtr), NULL) } |]
    fmap fromIntegral <$> peekArray ndim dimsPtr

h5s_select_hyperslab :: Hid -> [Int] -> [Int] -> [Int] -> IO ()
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
        !_ <-
          checkError
            =<< [C.exp| herr_t {
                  H5Sselect_hyperslab($(hid_t dspace), H5S_SELECT_SET,
                                      $(const hsize_t* c_start),
                                      $(const hsize_t* c_stride),
                                      $(const hsize_t* c_size),
                                      NULL)
                } |]
        return ()

toUnsigned :: (Show a, Integral a, Integral b) => a -> b
toUnsigned x
  | x < 0 = error $ "negative size or stride: " <> show x
  | otherwise = fromIntegral x

simpleStrides :: [Int] -> [Int]
simpleStrides = drop 1 . foldr acc [1]
  where
    acc x (y : ys) = let !r = x * y in (r : y : ys)
    acc _ _ = error "bug: this should never happen"

getMaxDims :: [Int] -> [Int] -> [Int]
getMaxDims dims strides
  | length dims == length strides = foldr acc [] $ zipWith (*) dims strides
  | otherwise = error "lengths of dims and strides do not match"
  where
    acc x (y : ys) = let !r = x `div` y in (r : y : ys)
    acc x [] = [x]

-- }}}
----------------------------------------------------------------------------------------------------
-- Datassets
----------------------------------------------------------------------------------------------------
-- {{{

h5d_open :: HasCallStack => Hid -> Text -> IO Hid
h5d_open parent name =
  checkError
    =<< [C.exp| hid_t { H5Dopen($(hid_t parent), $bs-ptr:c_name, H5P_DEFAULT) } |]
  where
    c_name = encodeUtf8 name

h5d_create :: HasCallStack => Hid -> Text -> Hid -> Hid -> IO Hid
h5d_create parent name dtype dspace =
  checkError
    =<< [C.exp| hid_t { H5Dcreate($(hid_t parent), $bs-ptr:c_name, $(hid_t dtype), $(hid_t dspace),
                                  H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) } |]
  where
    c_name = encodeUtf8 name

h5d_get_space :: HasCallStack => Hid -> IO Hid
h5d_get_space dataset = checkError =<< [C.exp| hid_t { H5Dget_space($(hid_t dataset)) } |]

h5d_get_type :: HasCallStack => Hid -> IO Hid
h5d_get_type dataset = checkError =<< [C.exp| hid_t { H5Dget_type($(hid_t dataset)) } |]

h5d_read :: forall a. (HasCallStack, KnownDataset a) => Hid -> IO a
h5d_read dataset =
  bracket (h5d_get_space dataset) h5s_close $ \dataset_dspace ->
    bracket (h5d_get_type dataset) h5o_close $ \dataset_dtype -> do
      sizeInBytes <-
        (*) <$> (product <$> h5s_get_simple_extent_dims dataset_dspace)
          <*> (h5t_get_size dataset_dtype)
      buffer <- MV.unsafeNew sizeInBytes
      !_ <- MV.unsafeWith buffer $ \bufferPtr ->
        checkError
          =<< [C.exp| herr_t { H5Dread($(hid_t dataset), $(hid_t dataset_dtype), $(hid_t dataset_dspace),
                                       $(hid_t dataset_dspace), H5P_DEFAULT,
                                       (void*)$(uint8_t* bufferPtr)) } |]
      peekArrayView $ ArrayView (Datatype dataset_dtype) (Dataspace dataset_dspace) buffer

h5d_write :: forall a. (HasCallStack, KnownDataset a) => Hid -> a -> IO ()
h5d_write dataset value =
  bracket (h5d_get_space dataset) h5s_close $ \dataset_dspace ->
    bracket (h5d_get_type dataset) h5o_close $ \dataset_dtype ->
      withArrayView value $ \(ArrayView (Datatype object_dtype) (Dataspace object_dspace) buffer) -> do
        checkDatatype (Datatype object_dtype) (Datatype dataset_dtype)
        !_ <- MV.unsafeWith buffer $ \bufferPtr ->
          checkError
            =<< [C.exp| herr_t { H5Dwrite($(hid_t dataset), $(hid_t object_dtype), $(hid_t object_dspace),
                                          $(hid_t dataset_dspace), H5P_DEFAULT,
                                          (const void*)$(const uint8_t* bufferPtr)) } |]
        return ()

instance (Storable a, KnownDatatype a) => KnownDataset [a] where
  withArrayView xs = withArrayView (V.fromList xs)
  peekArrayView view = V.toList <$> peekArrayView view

instance (Storable a, KnownDatatype a) => KnownDataset (Vector a) where
  withArrayView v = withArrayView ([V.length v], v)
  peekArrayView view =
    peekArrayView view >>= \case
      ([_], v) -> return v
      (dims :: [Int], _) -> error $ "invalid shape: " <> show dims <> "; expected a one-dimensional array"

instance (Storable a, KnownDatatype a) => KnownDataset ([Int], Vector a) where
  withArrayView (dims, v) action = withArrayView (dims, simpleStrides dims, v) action

  -- withDatatype (Proxy @a) $ \dtype ->
  --   withSimpleDataspace dims $ \dspace ->
  --     V.unsafeThaw v >>= \buffer ->
  --       action $ ArrayView dtype dspace (MV.unsafeCast buffer)
  peekArrayView (ArrayView dtype (Dataspace dspace) buffer) = do
    withDatatype (Proxy @a) $ \object_dtype ->
      checkDatatype object_dtype dtype
    dims <- h5s_get_simple_extent_dims dspace
    size <- let (Datatype dt) = dtype in h5t_get_size dt
    unless (product dims * size == MV.length buffer) . error $
      "bug: buffer has wrong length: "
        <> show (MV.length buffer)
        <> "; dataspace has shape "
        <> show dims
        <> ", element size is "
        <> show size
    v <- V.unsafeFreeze $ MV.unsafeCast buffer
    return (dims, v)

instance (Storable a, KnownDatatype a) => KnownDataset ([Int], [Int], Vector a) where
  withArrayView (dims, strides, v) action =
    withDatatype (Proxy @a) $ \dtype ->
      withStridedDataspace dims strides $ \dspace ->
        V.unsafeThaw v >>= \buffer ->
          action $ ArrayView dtype dspace (MV.unsafeCast buffer)
  peekArrayView view =
    peekArrayView view >>= \(dims, v) -> return (dims, simpleStrides dims, v)

-- }}}
----------------------------------------------------------------------------------------------------
-- Attributes
----------------------------------------------------------------------------------------------------
-- {{{
h5a_open :: Hid -> Text -> IO Hid
h5a_open object name = checkError =<< [C.exp| hid_t { H5Aopen($(hid_t object), $bs-ptr:c_name, H5P_DEFAULT) } |]
  where
    c_name = encodeUtf8 name

h5a_create :: Hid -> Text -> Hid -> IO Hid
h5a_create object name dtype =
  withScalarDataspace $ \(Dataspace dspace) ->
    checkError
      =<< [C.exp| hid_t {
            H5Acreate($(hid_t object), $bs-ptr:c_name, $(hid_t dtype), $(hid_t dspace),
                                   H5P_DEFAULT, H5P_DEFAULT) } |]
  where
    c_name = encodeUtf8 name

h5a_close :: HasCallStack => Hid -> IO ()
h5a_close attr = void . checkError =<< [C.exp| herr_t { H5Aclose($(hid_t attr)) } |]

h5a_get_type :: HasCallStack => Hid -> IO Hid
h5a_get_type attr = checkError =<< [C.exp| hid_t { H5Aget_type($(hid_t attr)) } |]

h5a_read :: forall a. (HasCallStack, KnownDatatype a) => Hid -> Text -> IO a
h5a_read object name =
  bracket (h5a_open object name) h5a_close $ \attr ->
    bracket (h5a_get_type attr) h5o_close $ \attr_dtype ->
      withDatatype (Proxy @a) $ \(Datatype object_dtype) -> do
        h5t_equal attr_dtype object_dtype >>= \case
          True ->
            h5t_get_size object_dtype >>= \objectSize ->
              allocaBytes objectSize $ \buffer ->
                [C.exp| herr_t { H5Aread($(hid_t attr), $(hid_t object_dtype), $(void* buffer)) } |]
                  >>= checkError >> h5Peek buffer objectSize
          False -> do
            name1 <- h5hl_dtype_to_text object_dtype
            name2 <- h5hl_dtype_to_text attr_dtype
            error $
              "tried to read attribute of type "
                <> name1
                <> ", but the attribute in file has type "
                <> name2

h5a_write :: forall a. (HasCallStack, KnownDatatype a) => Hid -> Text -> a -> IO ()
h5a_write object name value =
  withDatatype (Proxy @a) $ \(Datatype object_dtype) -> do
    alreadyExists <- h5a_exists object name
    let acquire =
          if alreadyExists
            then h5a_open object name
            else h5a_create object name object_dtype
    bracket acquire h5a_close $ \attr -> do
      bracket (h5a_get_type attr) h5o_close $ \attr_dtype ->
        h5t_equal attr_dtype object_dtype >>= \case
          True ->
            h5With value $ \buffer _ ->
              [C.exp| herr_t { H5Awrite($(hid_t attr), $(hid_t object_dtype), $(void* buffer)) } |]
                >>= checkError >> return ()
          False -> do
            name1 <- h5hl_dtype_to_text object_dtype
            name2 <- h5hl_dtype_to_text attr_dtype
            error $
              "tried to write attribute of type "
                <> name1
                <> ", but the attribute in file has type "
                <> name2

h5a_exists :: HasCallStack => Hid -> Text -> IO Bool
h5a_exists object name = fromHtri =<< [C.exp| htri_t { H5Aexists($(hid_t object), $bs-ptr:c_name) } |]
  where
    c_name = encodeUtf8 name

h5a_delete :: HasCallStack => Hid -> Text -> IO ()
h5a_delete object name = void . checkError =<< [C.exp| herr_t { H5Adelete($(hid_t object), $bs-ptr:c_name) } |]
  where
    c_name = encodeUtf8 name

-- }}}
