{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.HDF5.Internal where

import Control.Exception.Safe hiding (handle)
import Data.ByteString (useAsCString)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types hiding (CSize)
import Foreign.Marshal.Alloc (alloca, allocaBytesAligned)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.Storable (Storable (..))
import qualified GHC.Show
import Relude hiding (error)
import System.IO.Unsafe (unsafePerformIO)
import Prelude (error)

#include <hdf5.h>
#include <hdf5_hl.h>

type Hid = {#type hid_t#}
type Herr = {#type herr_t#}
type Hsize = {#type hsize_t#}
type Hssize = {#type hssize_t#}
type Htri = {#type htri_t#}
type Hbool = {#type hbool_t#}

type CSize = {#type size_t#}

{#typedef herr_t Herr#}
{#typedef hid_t Hid#}
-- {#typedef hsize_t Hsize#}
-- {#typedef hssize_t Hssize#}
{#typedef htri_t Htri#}
{#typedef hbool_t Hbool#}

data H5P_DEFAULT = H5P_DEFAULT

instance Enum H5P_DEFAULT where
  toEnum = error "Enum.toEnum is not defined for H5P_DEFAULT"
  fromEnum H5P_DEFAULT = 0

{#default in `H5P_DEFAULT' [hid_t] withEnum#}

h5t_VARIABLE :: CSize
-- #define H5T_VARIABLE    ((size_t)(-1))  /* Indicate that a string is variable length (null-terminated in C, instead of fixed length) */
h5t_VARIABLE = maxBound

h5s_ALL :: Hid
-- #define H5S_ALL         (hid_t)0
h5s_ALL = 0

data H5F_ACC
  = H5F_ACC_RDONLY
  | H5F_ACC_RDWR
  | H5F_ACC_TRUNC
  | H5F_ACC_EXCL
  | H5F_ACC_DEBUG
  | H5F_ACC_CREAT
  | H5F_ACC_SWMR_WRITE
  | H5F_ACC_SWMR_READ

instance Enum H5F_ACC where
  toEnum = error "Enum.toEnum is not defined for H5F_ACC"
  fromEnum = \case
    H5F_ACC_RDONLY -> 0x0000
    H5F_ACC_RDWR -> 0x0001
    H5F_ACC_TRUNC -> 0x0002
    H5F_ACC_EXCL -> 0x0004
    H5F_ACC_DEBUG -> 0x0008
    H5F_ACC_CREAT -> 0x0010
    H5F_ACC_SWMR_WRITE -> 0x0020
    H5F_ACC_SWMR_READ -> 0x0040

{#enum H5_iter_order_t {} #}
{#enum H5_index_t {} #}
{#enum H5T_class_t {} deriving(Eq, Show) #}
{#enum H5T_cset_t {} #}
{#enum H5E_direction_t {} #}
{#enum H5S_class_t {} #}
{#enum H5LT_lang_t {} #}
{#enum H5O_type_t {} deriving(Show) #}

withEnum :: (Enum a, Integral b) => a -> b
withEnum = fromIntegral . fromEnum


-- | A tag type for 'Object' GADT. Allows us to have polymorphic algorithms
-- while keeping everything type-safe.
data ObjectType = FileTy | GroupTy | DatasetTy | DatatypeTy

-- | A HDF5 object.
--
-- Many operations in HDF5 library
data Object (k :: ObjectType) where
  File :: Hid -> Object 'FileTy
  Group :: Hid -> Object 'GroupTy
  Dataset :: Hid -> Object 'DatasetTy
  Datatype :: Hid -> Object 'DatatypeTy

type File = Object 'FileTy

type Group = Object 'GroupTy

type Dataset = Object 'DatasetTy

type Datatype = Object 'DatatypeTy


class MkObject (t :: ObjectType) where
  unsafeMkObject :: Hid -> Object t

instance MkObject 'FileTy where unsafeMkObject = File
instance MkObject 'GroupTy where unsafeMkObject = Group
instance MkObject 'DatasetTy where unsafeMkObject = Dataset
instance MkObject 'DatatypeTy where unsafeMkObject = Datatype

--------------------------------------------------------------------------------
-- Error Handling
--------------------------------------------------------------------------------

type H5E_walk2_t = CUInt -> Ptr ErrorInfo -> Ptr () -> IO Herr
type H5E_auto2_t = Hid -> Ptr () -> IO Herr

foreign import ccall "wrapper"
  mkWalk :: H5E_walk2_t -> IO (FunPtr H5E_walk2_t)

-- typedef struct H5E_error2_t {
--     hid_t       cls_id;    /*class ID                           */
--     hid_t       maj_num;   /*major error ID		     */
--     hid_t       min_num;   /*minor error number		     */
--     unsigned    line;      /*line in file where error occurs    */
--     const char *func_name; /*function in which error occurred   */
--     const char *file_name; /*file in which error occurred       */
--     const char *desc;      /*optional supplied description      */
-- } H5E_error2_t;
data ErrorInfo = ErrorInfo
  { errorInfoClass :: {-# UNPACK #-} !Hid,
    errorInfoMajor :: {-# UNPACK #-} !Int,
    errorInfoMinor :: {-# UNPACK #-} !Int,
    errorInfoLine :: {-# UNPACK #-} !Int,
    errorInfoFunc :: {-# UNPACK #-} !Text,
    errorInfoFile :: {-# UNPACK #-} !Text,
    errorInfoDesc :: {-# UNPACK #-} !(Maybe Text)
  }
  deriving stock (Show)

instance Storable ErrorInfo where
  sizeOf _ = {#sizeof H5E_error2_t#}
  alignment _ = {#alignof H5E_error2_t#}
  peek p = ErrorInfo <$> {#get H5E_error2_t->cls_id#} p
                     <*> (fromIntegral <$> {#get H5E_error2_t->maj_num#} p)
                     <*> (fromIntegral <$> {#get H5E_error2_t->min_num#} p)
                     <*> (fromIntegral <$> {#get H5E_error2_t->line#} p)
                     <*> (fmap toText $ peekCString =<< {#get H5E_error2_t->func_name#} p)
                     <*> (fmap toText $ peekCString =<< {#get H5E_error2_t->file_name#} p)
                     <*> (maybeDesc =<< {#get H5E_error2_t->desc#} p)
   where maybeDesc !p'
          | p' == nullPtr = return Nothing
          | otherwise = Just . toText <$> peekCString p'
  poke _ _ = error "Storable.poke is undefined for ErrorInfo"

-- hid_t H5Eget_current_stack(void)
{#fun H5Eget_current_stack as h5e_get_current_stack { } -> `Hid' #}
-- herr_t H5Eclose_stack(hid_t estack_id)
{#fun H5Eclose_stack as h5e_close_stack { `Hid' } -> `Herr' #}
-- herr_t H5Eprint2( hid_t estack_id, FILE * stream)
{#fun H5Eprint2 as h5e_print { `Hid', `Ptr ()' } -> `Herr' #}
-- herr_t H5Eprint2( hid_t estack_id, FILE * stream)
{#fun H5Ewalk2 as h5e_walk { `Hid', `H5E_direction_t', castFunPtr `FunPtr H5E_walk2_t', `Ptr ()' } -> `Herr' #}
-- herr_t H5Eset_auto2( hid_t estack_id, H5E_auto2_t func, void *client_data )
{#fun H5Eset_auto2 as h5e_set_auto { `Hid', id `FunPtr H5E_auto2_t', `Ptr ()' } -> `Herr' #}

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
collectStack = liftIO . uninterruptibleMask_ $
  bracket acquire release $ \stackId -> do
    (listRef :: IORef [ErrorInfo]) <- newIORef []
    let callback _ ptr _ = peek ptr >>= \info -> modifyIORef listRef ((:) info) >> return 0
    bracket (mkWalk callback) freeHaskellFunPtr $ \fn -> do
      status <- h5e_walk stackId H5E_WALK_UPWARD fn nullPtr
      when (status < 0) . error $ "H5Ewalk2 failed with error code " <> show status
    readIORef listRef
  where
    acquire = do
      h <- h5e_get_current_stack
      when (h < 0) . error $ "H5Eget_current_stack failed with error code " <> show h
      return h
    release h = do
      status <- h5e_close_stack h
      when (status < 0) . error $ "H5Eclose_stack failed with error code " <> show status

disableDiagOutput :: MonadIO m => m ()
disableDiagOutput = do
  status <- liftIO $ h5e_set_auto {-H5E_DEFAULT-} 0 nullFunPtr nullPtr
  when (status < 0) . error $ "H5Eset_auto2 failed with error code " <> show status

h5Fail :: (Integral a, MonadIO m, MonadThrow m) => Maybe Text -> a -> m b
h5Fail !msg !code = collectStack >>= \stack -> throw (H5Exception (fromIntegral code) stack msg)

h5Check :: (Integral a, MonadIO m, MonadThrow m) => Maybe Text -> a -> m a
h5Check msg !code = when (code < 0) (h5Fail msg code) >> return code

liftCheck :: (Integral a, MonadIO m, MonadThrow m) => Text -> IO a -> m a
liftCheck msg action = h5Check (Just msg) =<< liftIO action


_checkError :: Integral a => a -> IO a
_checkError code = when (code < 0) (h5Fail Nothing code) >> return code

_createObject :: MkObject t => Hid -> IO (Object t)
_createObject c = unsafeMkObject <$> _checkError c

_toBool :: Htri -> IO Bool
_toBool c = (> 0) <$> _checkError c

withText :: Text -> (CString -> IO a) -> IO a
withText text = useAsCString (encodeUtf8 text)

-- hid_t H5Fopen( const char *name, unsigned flags, hid_t fapl_id )
{#fun H5Fopen as h5f_open { withText* `Text', withEnum `H5F_ACC', `H5P_DEFAULT' } -> `File' _createObject* #}
-- hid_t H5Fcreate( const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id )
{#fun H5Fcreate as h5f_create
  { withText* `Text', withEnum `H5F_ACC', `H5P_DEFAULT', `H5P_DEFAULT' } -> `File' _createObject* #}
-- herr_t H5Fclose( hid_t file_id )
{#fun H5Fclose as h5f_close { `Hid' } -> `Herr' _checkError*- #}

-- hid_t H5Oopen( hid_t loc_id, const char *name, hid_t lapl_id )
{#fun H5Oopen as h5o_open { `Hid', withText* `Text', `H5P_DEFAULT' } -> `Hid' _checkError* #}
-- hid_t H5Oopen_by_idx( hid_t loc_id, const char *group_name, H5_index_t index_type, H5_iter_order_t order, hsize_t n, hid_t lapl_id )
{#fun H5Oopen_by_idx as h5o_open_by_idx
  { `Hid', withText* `Text', `H5_index_t', `H5_iter_order_t', `Int', `H5P_DEFAULT' } -> `Hid' _checkError* #}
{#fun H5Oclose as h5o_close { `Hid' } -> `Herr' _checkError*- #}

#if H5_VERS_MAJOR == 1 && (H5_VERS_MINOR < 10 || (H5_VERS_MINOR == 10 && H5_VERS_RELEASE <= 3))
-- herr_t H5Oget_info ( hid_t loc_id, H5O_info_t *oinfo )
{#fun H5Oget_info as h5o_get_info' { `Hid', `Ptr ()' } -> `()' _checkError*- #}

h5o_get_type :: Hid -> IO H5O_type_t
h5o_get_type h =
  allocaBytesAligned {#sizeof H5O_info_t#} {#alignof H5O_info_t#} $ \ptr -> do
    h5o_get_info' h ptr
    toEnum <$> (fromIntegral :: CInt -> Int) <$> peekByteOff ptr {#offsetof H5O_info_t->type#}
#else
-- herr_t H5Oget_info2 ( hid_t loc_id, H5O_info2_t *oinfo, unsigned fields )
{#fun H5Oget_info2 as h5o_get_info' { `Hid', `Ptr ()', `CUInt' } -> `()' _checkError*- #}

h5o_get_type :: Hid -> IO H5O_type_t
h5o_get_type h =
  allocaBytesAligned {#sizeof H5O_info_t#} {#alignof H5O_info_t#} $ \ptr -> do
    h5o_get_info' h ptr h5o_INFO_BASIC
    toEnum <$> (fromIntegral :: CInt -> Int) <$> peekByteOff ptr {#offsetof H5O_info_t->type#}
  where h5o_INFO_BASIC :: CUInt
        h5o_INFO_BASIC = 1
#endif

-- htri_t H5Iis_valid( hid_t obj_id )
{#fun H5Iis_valid as h5i_is_valid { `Hid' } -> `Bool' _toBool* #}
-- ssize_t H5Iget_name( hid_t obj_id, char *name, size_t size )
{#fun H5Iget_name as h5i_get_name { `Hid', id `Ptr CChar', `Int' } -> `CLong' _checkError* #}
-- hid_t H5Iget_file_id( hid_t obj_id )
{#fun H5Iget_file_id as h5i_get_file_id { `Hid' } -> `File' _createObject* #}

-- herr_t H5Gget_info( hid_t group_id, H5G_info_t *group_info )
{#fun H5Gget_info as h5g_get_info' { `Hid', `Ptr ()' } -> `Herr' _checkError*- #}
-- H5Gget_num_objs is deprecated in favour of H5Gget_info, so we use it to
-- obtain the number of objects within a group. Note that this function uses
-- error codes: negative return value indicates error.
h5g_get_num_objs :: Hid -> IO Int
h5g_get_num_objs groupId =
  allocaBytesAligned {#sizeof H5G_info_t#} {#alignof H5G_info_t#} $ \ptr -> do
    h5g_get_info' groupId ptr
    (count :: {#type hsize_t#}) <- peekByteOff ptr {#offsetof H5G_info_t->nlinks#}
    return $ fromIntegral count
-- hid_t H5Gcreate2( hid_t loc_id, const char *name, hid_t lcpl_id, hid_t gcpl_id, hid_t gapl_id )
{#fun H5Gcreate2 as h5g_create
  { `Hid', withText* `Text', `H5P_DEFAULT', `H5P_DEFAULT', `H5P_DEFAULT' } -> `Group' _createObject* #}

-- herr_t H5Ldelete( hid_t loc_id, const char *name, hid_t lapl_id )
{#fun H5Ldelete as h5l_delete { `Hid', withText* `Text', `H5P_DEFAULT' } -> `()' _checkError*- #}
-- htri_t H5Lexists( hid_t loc_id, const char *name, hid_t lapl_id )
{#fun H5Lexists as h5l_exists { `Hid', withText* `Text', `H5P_DEFAULT' } -> `Bool' _toBool* #}

-- hid_t H5Dget_type(hid_t dataset_id )
{#fun H5Dget_type as h5d_get_type { `Hid' } -> `Datatype' _createObject* #}
-- hid_t H5Dopen2( hid_t loc_id, const char *name, hid_t dapl_id )
-- {#fun H5Dopen2 as h5d_open { `Hid', withText* `Text', `H5P_DEFAULT' } -> `Dataset' _createObject* #}
-- herr_t H5Dclose( hid_t dataset_id )
-- {#fun H5Dclose as h5d_close { `Hid' } -> `()' _checkError*- #}
-- herr_t H5Dread(hid_t dataset_id, hid_t mem_type_id, hid_t mem_space_id,
--                hid_t file_space_id, hid_t xfer_plist_id, void * buf)
{#fun H5Dread as h5d_read
  { `Hid', `Hid', `Hid', `Hid', `H5P_DEFAULT', `Ptr ()' } -> `()' _checkError*- #}
-- herr_t H5Dwrite(hid_t dataset_id, hid_t mem_type_id, hid_t mem_space_id,
--                 hid_t file_space_id, hid_t xfer_plist_id, const void * buf)
{#fun H5Dwrite as h5d_write
  { `Hid', `Hid', `Hid', `Hid', `H5P_DEFAULT', `Ptr ()' } -> `()' _checkError*- #}

-- hid_t H5Dget_space(hid_t dataset_id)
{#fun H5Dget_space as h5d_get_space { `Hid' } -> `Hid' _checkError* #}
-- hid_t H5Dcreate2(hid_t loc_id, const char *name, hid_t dtype_id, hid_t space_id, hid_t lcpl_id, hid_t dcpl_id, hid_t dapl_id)
{#fun H5Dcreate2 as h5d_create2
  { `Hid', withText* `Text', `Hid', `Hid', `H5P_DEFAULT', `H5P_DEFAULT', `H5P_DEFAULT'
    } -> `Dataset' _createObject* #}

-- hid_t H5Tcreate( H5T_class_t class, size_t size )
{#fun H5Tcreate as h5t_create { `H5T_class_t', `Int' } -> `Datatype' _createObject* #}
-- herr_t H5Tinsert( hid_t dtype_id, const char * name, size_t offset, hid_t field_id )
{#fun H5Tinsert as h5t_insert { `Hid', withText* `Text', `Int', `Hid' } -> `()' _checkError*- #}
-- hid_t H5Tcopy( hid_t dtype_id )
{#fun H5Tcopy as h5t_copy { `Hid' } -> `Datatype' _createObject* #}
-- herr_t H5Tset_cset( hid_t dtype_id, H5T_cset_t cset )
{#fun H5Tset_cset as h5t_set_cset { `Hid', `H5T_cset_t' } -> `()' _checkError*- #}
-- herr_t H5Tset_size( hid_t dtype_id, size_t size )
{#fun H5Tset_size as h5t_set_size { `Hid', id `CSize' } -> `()' _checkError*- #}
-- size_t H5Tget_size( hid_t dtype_id )
{#fun H5Tget_size as h5t_get_size { `Hid' } -> `Int' _check* #}
  where _check :: CSize -> IO Int
        _check n
          | n > 0 = return (fromIntegral n)
          | otherwise = h5Fail (Just "failed to obtain datatype size") (-1 :: Int)
-- herr_t H5Tclose( hid_t dtype_id )
-- {#fun H5Tclose as h5t_close { `Hid' } -> `()' _checkError*- #}
-- htri_t H5Tequal( hid_t dtype_id1, hid_t dtype_id2 )
{#fun H5Tequal as h5t_equal { `Hid', `Hid' } -> `Bool' _toBool* #}

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

-- hid_t H5Screate(H5S_class_t type)
{#fun H5Screate as h5s_create { `H5S_class_t' } -> `Hid' _checkError* #}
-- herr_t H5Sclose(hid_t space_id)
{#fun H5Sclose as h5s_close { `Hid' } -> `()' _checkError*- #}
-- int H5Sget_simple_extent_ndims(hid_t space_id)
{#fun H5Sget_simple_extent_ndims as h5s_get_simple_extent_ndims { `Hid' } -> `CInt' _checkError* #}
-- int H5Sget_simple_extent_dims(hid_t space_id, hsize_t *dims, hsize_t *maxdims)
{#fun H5Sget_simple_extent_dims as h5s_get_simple_extent_dims { `Hid', id `Ptr Hsize', id `Ptr Hsize' } -> `()' _checkError*- #}
-- herr_t H5Sset_extent_simple(hid_t space_id, int rank, const hsize_t *current_size, const hsize_t *maximum_size)
{#fun H5Sset_extent_simple as h5s_set_extent_simple
  { `Hid', `Int', id `Ptr Hsize', id `Ptr Hsize' } -> `()' _checkError*- #}



-- hid_t H5Aopen(hid_t obj_id, const char *attr_name, hid_t aapl_id)
{#fun H5Aopen as h5a_open { `Hid', withText* `Text', `H5P_DEFAULT' } -> `Hid' _checkError* #}
-- herr_t H5Aclose(hid_t attr_id)
{#fun H5Aclose as h5a_close { `Hid' } -> `()' _checkError*- #}
-- hid_t H5Acreate2(hid_t loc_id, const char *attr_name, hid_t type_id, hid_t space_id, hid_t acpl_id, hid_t aapl_id)
{#fun H5Acreate2 as h5a_create2
  { `Hid', withText* `Text', `Hid', `Hid', `H5P_DEFAULT', `H5P_DEFAULT' } -> `Hid' _checkError* #}
-- herr_t H5Adelete(hid_t loc_id, const char *attr_name)
{#fun H5Adelete as h5a_delete { `Hid', withText* `Text' } -> `()' _checkError*- #}
-- hid_t H5Aget_type(hid_t attr_id)
{#fun H5Aget_type as h5a_get_type { `Hid' } -> `Datatype' _createObject* #}
-- htri_t H5Aexists(hid_t obj_id, const char *attr_name)
-- {#fun H5Aexists as h5a_exists { `Hid', `String' } -> `Htri' #}
{#fun H5Aexists as h5a_exists { `Hid', withText* `Text' } -> `Bool' _toBool* #}
-- herr_t H5Aread(hid_t attr_id, hid_t mem_type_id, void *buf)
{# fun H5Aread as h5a_read { `Hid', `Hid', `Ptr ()' } -> `()' _checkError*- #}
-- herr_t H5Awrite(hid_t attr_id, hid_t mem_type_id, const void *buf)
{# fun H5Awrite as h5a_write { `Hid', `Hid', `Ptr ()' } -> `()' _checkError*- #}
-- herr_t H5Aget_info(hid_t attr_id, H5A_info_t *ainfo)
{#fun H5Aget_info as h5a_get_info' { `Hid', `Ptr ()' } -> `()' _checkError*- #}
h5a_get_data_size :: Hid -> IO Int
h5a_get_data_size attrId =
  allocaBytesAligned {#sizeof H5A_info_t#} {#alignof H5A_info_t#} $ \ptr -> do
    h5a_get_info' attrId ptr
    (count :: {#type hsize_t#}) <- peekByteOff ptr {#offsetof H5A_info_t->data_size#}
    putStrLn $ "count = " <> show count
    return $ fromIntegral count

-- herr_t H5LTfind_dataset ( hid_t loc_id, const char *dset_name )
{#fun H5LTfind_dataset as h5lt_find_dataset { `Hid', `String' } -> `Herr' #}
-- herr_t H5LTget_dataset_ndims ( hid_t loc_id, const char *dset_name, int *rank )
{#fun H5LTget_dataset_ndims as h5lt_get_dataset_ndims { `Hid', `String', id `Ptr CInt' } -> `Herr' #}
-- herr_t H5LTget_dataset_info ( hid_t loc_id, const char *dset_name, hsize_t *dims, H5T_class_t *class_id, size_t *type_size )
-- NOTE: underlying type of C enum is int, so we cheat a little
{#fun H5LTget_dataset_info as h5lt_get_dataset_info { `Hid', `String', id `Ptr Hsize', id `Ptr CInt', id `Ptr CSize' } -> `Herr' #}
-- herr_t H5LTdtype_to_text(hid_t datatype, char* str, H5LT_lang_t lang_type, size_t* len)
{#fun H5LTdtype_to_text as h5lt_dtype_to_text { `Hid', id `Ptr CChar', `H5LT_lang_t', id `Ptr CSize' } -> `Herr' #}

-- herr_t H5LTread_dataset ( hid_t loc_id, const char *dset_name, hid_t type_id, void *buffer )
{#fun H5LTread_dataset as h5lt_read_dataset { `Hid', `String', `Hid', `Ptr ()' } -> `Herr' #}
-- herr_t H5LTread_dataset_char ( hid_t loc_id, const char *dset_name, char *buffer )
-- {#fun H5LTread_dataset_char as h5lt_read_dataset_char { `Hid', `String', id `Ptr CChar' } -> `Herr' #}
-- herr_t H5LTread_dataset_short ( hid_t loc_id, const char *dset_name, short *buffer )
-- {#fun H5LTread_dataset_short as h5lt_read_dataset_short { `Hid', `String', id `Ptr CShort' } -> `Herr' #}
-- herr_t H5LTread_dataset_float ( hid_t loc_id, const char *dset_name, float *buffer )
-- {#fun H5LTread_dataset_float as h5lt_read_dataset_float { `Hid', `String', id `Ptr CFloat' } -> `Herr' #}
-- herr_t H5LTread_dataset_double ( hid_t loc_id, const char *dset_name, double *buffer )
-- {#fun H5LTread_dataset_double as h5lt_read_dataset_double { `Hid', `String', id `Ptr CDouble' } -> `Herr' #}
-- herr_t H5LTread_dataset_int ( hid_t loc_id, const char *dset_name, int *buffer )
-- {#fun H5LTread_dataset_int as h5lt_read_dataset_int { `Hid', `String', id `Ptr CInt' } -> `Herr' #}
-- herr_t H5LTread_dataset_long ( hid_t loc_id, const char *dset_name, long *buffer )
-- {#fun H5LTread_dataset_long as h5lt_read_dataset_long { `Hid', `String', id `Ptr CLong' } -> `Herr' #}
-- herr_t H5LTread_dataset_string ( hid_t loc_id, const char *dset_name, char *buffer )
-- {#fun H5LTread_dataset_string as h5lt_read_dataset_string { `Hid', `String', id `Ptr CChar' } -> `Herr' #}

-- herr_t H5LTmake_dataset ( hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, hid_t type_id, const void*buffer )
{#fun H5LTmake_dataset as h5lt_make_dataset { `Hid', `String', `Int', id `Ptr Hsize', `Hid', id `Ptr ()' } -> `Herr' #}
-- herr_t H5LTmake_dataset_char ( hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const char *buffer )
-- {#fun H5LTmake_dataset_char as h5lt_make_dataset_char { `Hid', `String', `CInt', id `Ptr Hsize', id `Ptr CChar' } -> `Herr' #}
-- herr_t H5LTmake_dataset_short ( hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const short *buffer )
-- {#fun H5LTmake_dataset_short as h5lt_make_dataset_short { `Hid', `String', `CInt', id `Ptr Hsize', id `Ptr CShort' } -> `Herr' #}
-- herr_t H5LTmake_dataset_int ( hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const int *buffer )
-- {#fun H5LTmake_dataset_int as h5lt_make_dataset_int { `Hid', `String', `CInt', id `Ptr Hsize', id `Ptr CInt' } -> `Herr' #}
-- herr_t H5LTmake_dataset_long ( hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const long *buffer )
-- {#fun H5LTmake_dataset_long as h5lt_make_dataset_long { `Hid', `String', `CInt', id `Ptr Hsize', id `Ptr CLong' } -> `Herr' #}
-- herr_t H5LTmake_dataset_float ( hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const float *buffer )
-- {#fun H5LTmake_dataset_float as h5lt_make_dataset_float { `Hid', `String', `CInt', id `Ptr Hsize', id `Ptr CFloat' } -> `Herr' #}
-- herr_t H5LTmake_dataset_double ( hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const double*buffer )
-- {#fun H5LTmake_dataset_double as h5lt_make_dataset_double { `Hid', `String', `CInt', id `Ptr Hsize', id `Ptr CDouble' } -> `Herr' #}

-- herr_t H5LTget_attribute( hid_t loc_id, const char *obj_name, const char *attr_name,  hid_t mem_type_id, void *data )
-- {#fun H5LTget_attribute as h5lt_get_attribute { `Hid', `String', `String', `Hid', `Ptr ()' } -> `Herr' #}

foreign import ccall unsafe "&H5T_NATIVE_INT64_g" h5t_NATIVE_INT64 :: Ptr Hid
foreign import ccall unsafe "&H5T_NATIVE_FLOAT_g" h5t_NATIVE_FLOAT :: Ptr Hid
foreign import ccall unsafe "&H5T_NATIVE_DOUBLE_g" h5t_NATIVE_DOUBLE :: Ptr Hid
foreign import ccall unsafe "&H5T_C_S1_g" h5t_C_S1 :: Ptr Hid
