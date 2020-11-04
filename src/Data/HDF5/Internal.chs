{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.HDF5.Internal where

import Relude hiding (error)
import Prelude (error)
import Foreign.Ptr (Ptr, FunPtr, nullPtr, plusPtr, castFunPtr)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Storable (Storable(..))
import Foreign.C.Types hiding (CSize)
import Foreign.C.String (peekCString)

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
  toEnum = undefined
  fromEnum H5P_DEFAULT = 0

{#default in `H5P_DEFAULT' [hid_t] withEnum#}


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
  toEnum = undefined
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
{#enum H5E_direction_t {} #}
{#enum H5LT_lang_t {} #}
{#enum H5O_type_t {} #}

withEnum :: (Enum a, Integral b) => a -> b
withEnum = fromIntegral . fromEnum

-- hid_t H5Fopen( const char *name, unsigned flags, hid_t fapl_id )
{#fun H5Fopen as h5f_open { `String', withEnum `H5F_ACC', `H5P_DEFAULT' } -> `Hid' #}
-- hid_t H5Fcreate( const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id )
{#fun H5Fcreate as h5f_create { `String', withEnum `H5F_ACC', `H5P_DEFAULT', `H5P_DEFAULT' } -> `Hid' #}
-- herr_t H5Fclose( hid_t file_id )
{#fun H5Fclose as h5f_close { `Hid' } -> `Herr' #}
-- hid_t H5Oopen( hid_t loc_id, const char *name, hid_t lapl_id )
{#fun H5Oopen as h5o_open { `Hid', `String', `H5P_DEFAULT' } -> `Hid' #}
-- hid_t H5Oopen_by_idx( hid_t loc_id, const char *group_name, H5_index_t index_type, H5_iter_order_t order, hsize_t n, hid_t lapl_id )
{#fun H5Oopen_by_idx as h5o_open_by_idx { `Hid', `String', `H5_index_t', `H5_iter_order_t', `Int', `H5P_DEFAULT' } -> `Hid' #}
{#fun H5Oclose as h5o_close { `Hid' } -> `Herr' #}

#if H5_VERS_MAJOR == 1 && (H5_VERS_MINOR < 10 || (H5_VERS_MINOR == 10 && H5_VERS_RELEASE <= 3))
-- herr_t H5Oget_info ( hid_t loc_id, H5O_info_t *oinfo )
{#fun H5Oget_info as h5o_get_info' { `Hid', `Ptr ()' } -> `Herr' #}

h5o_get_type :: Hid -> IO (Either Herr H5O_type_t)
h5o_get_type h =
  allocaBytesAligned {#sizeof H5O_info_t#} {#alignof H5O_info_t#} $ \ptr -> do
    status <- h5o_get_info' h ptr
    if status < 0
      then return . Left $ status
      else Right . toEnum <$> (fromIntegral :: CInt -> Int) <$> peekByteOff ptr {#offsetof H5O_info_t->type#}
#else
-- herr_t H5Oget_info2 ( hid_t loc_id, H5O_info2_t *oinfo, unsigned fields )
{#fun H5Oget_info2 as h5o_get_info' { `Hid', `Ptr ()', `CUInt' } -> `Herr' #}

h5o_get_type :: Hid -> IO (Either Herr H5O_type_t)
h5o_get_type h =
  allocaBytesAligned {#sizeof H5O_info_t#} {#alignof H5O_info_t#} $ \ptr -> do
    status <- h5o_get_info' h ptr h5o_INFO_BASIC
    if status < 0
      then return . Left $ status
      else Right . toEnum <$> (fromIntegral :: CInt -> Int) <$> peekByteOff ptr {#offsetof H5O_info_t->type#}
  where h5o_INFO_BASIC :: CUInt
        h5o_INFO_BASIC = 1
#endif


-- htri_t H5Iis_valid( hid_t obj_id )
{#fun H5Iis_valid as h5i_is_valid { `Hid' } -> `Htri' #}
{#fun H5Iget_name as h5i_get_name { `Hid', id `Ptr CChar', `Int' } -> `Int' #}
-- herr_t H5Gget_info( hid_t group_id, H5G_info_t *group_info )
{#fun H5Gget_info as h5g_get_info' { `Hid', `Ptr ()' } -> `Herr' #}
-- H5Gget_num_objs is deprecated in favour of H5Gget_info, so we use it to
-- obtain the number of objects within a group. Note that this function uses
-- error codes: negative return value indicates error.
h5g_get_num_objs :: Hid -> IO Int
h5g_get_num_objs groupId =
  allocaBytesAligned {#sizeof H5G_info_t#} {#alignof H5G_info_t#} $ \ptr -> do
    status <- h5g_get_info' groupId ptr
    if status < 0
      then return $ fromIntegral status
      else do
        count <- peekByteOff ptr {#offsetof H5G_info_t->nlinks#} :: IO {#type hsize_t#}
        return $ fromIntegral count
-- hid_t H5Gcreate2( hid_t loc_id, const char *name, hid_t lcpl_id, hid_t gcpl_id, hid_t gapl_id )
{#fun H5Gcreate2 as h5g_create { `Hid', `String', `H5P_DEFAULT', `H5P_DEFAULT', `H5P_DEFAULT' } -> `Hid' #}
-- herr_t H5Ldelete( hid_t loc_id, const char *name, hid_t lapl_id )
{#fun H5Ldelete as h5l_delete { `Hid', `String', `H5P_DEFAULT' } -> `Herr' #}

type H5E_walk2_t = CUInt -> Ptr ErrorInfo -> Ptr () -> IO Herr
type H5E_auto2_t = Hid -> Ptr () -> IO Herr

foreign import ccall "wrapper"
  mkWalk :: H5E_walk2_t -> IO (FunPtr H5E_walk2_t)


-- typedef struct H5E_error2_t {
--     hid_t       cls_id;         /*class ID                           */
--     hid_t       maj_num;	/*major error ID		     */
--     hid_t       min_num;	/*minor error number		     */
--     unsigned	line;		/*line in file where error occurs    */
--     const char	*func_name;   	/*function in which error occurred   */
--     const char	*file_name;	/*file in which error occurred       */
--     const char	*desc;		/*optional supplied description      */
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
  poke _ _ = undefined


printError :: H5E_walk2_t
printError n errInfo _ = do
  s <- peekCString =<< {#get H5E_error2_t->func_name#} errInfo
  print s
  return $ 0

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

-- hid_t H5Dget_type(hid_t dataset_id )
{#fun H5Dget_type as h5d_get_type { `Hid' } -> `Hid' #}
-- hid_t H5Dopen2( hid_t loc_id, const char *name, hid_t dapl_id )
{#fun H5Dopen2 as h5d_open { `Hid', `String', `H5P_DEFAULT' } -> `Hid' #}
-- herr_t H5Dclose( hid_t dataset_id )
{#fun H5Dclose as h5d_close { `Hid' } -> `Herr' #}

-- herr_t H5Tclose( hid_t dtype_id )
{#fun H5Tclose as h5t_close { `Hid' } -> `Herr' #}
-- htri_t H5Tequal( hid_t dtype_id1, hid_t dtype_id2 )
{#fun H5Tequal as h5t_equal { `Hid', `Hid' } -> `Htri' #}

-- hid_t H5Aopen(hid_t obj_id, const char *attr_name, hid_t aapl_id)
{#fun H5Aopen as h5a_open { `Hid', `String', `H5P_DEFAULT' } -> `Hid' #}
-- herr_t H5Aclose(hid_t attr_id)
{#fun H5Aclose as h5a_close { `Hid' } -> `Herr' #}
-- hid_t H5Aget_type(hid_t attr_id)
{#fun H5Aget_type as h5a_get_type { `Hid' } -> `Hid' #}

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
{#fun H5LTget_attribute as h5lt_get_attribute { `Hid', `String', `String', `Hid', `Ptr ()' } -> `Herr' #}

foreign import ccall unsafe "&H5T_NATIVE_FLOAT_g" h5t_NATIVE_FLOAT :: Ptr Hid
foreign import ccall unsafe "&H5T_NATIVE_DOUBLE_g" h5t_NATIVE_DOUBLE :: Ptr Hid
