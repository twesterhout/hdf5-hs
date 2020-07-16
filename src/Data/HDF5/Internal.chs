{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.HDF5.Internal where

import Relude hiding (error)
import Prelude (error)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Storable (Storable(..))
import Foreign.C.Types (CChar)

#include <hdf5.h>
#include <hdf5_hl.h>

type Hid = {#type hid_t#}
type Herr = {#type herr_t#}
type Hsize = {#type hsize_t#}
type Hssize = {#type hssize_t#}
type Htri = {#type htri_t#}
type Hbool = {#type hbool_t#}

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

withEnum :: (Enum a, Integral b) => a -> b
withEnum = fromIntegral . fromEnum

-- hid_t H5Fopen( const char *name, unsigned flags, hid_t fapl_id )
{#fun H5Fopen as h5f_open { `String', withEnum `H5F_ACC', `H5P_DEFAULT' } -> `Hid' #}
-- hid_t H5Fcreate( const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id )
{#fun H5Fcreate as h5f_create { `String', withEnum `H5F_ACC', `H5P_DEFAULT', `H5P_DEFAULT' } -> `Hid' #}
-- herr_t H5Fclose( hid_t file_id )
{#fun H5Fclose as h5f_close { `Hid' } -> `Herr' #}
-- hid_t H5Oopen_by_idx( hid_t loc_id, const char *group_name, H5_index_t index_type, H5_iter_order_t order, hsize_t n, hid_t lapl_id )
{#fun H5Oopen_by_idx as h5o_open_by_idx { `Hid', `String', `H5_index_t', `H5_iter_order_t', `Int', `H5P_DEFAULT' } -> `Hid' #}
{#fun H5Oclose as h5o_close { `Hid' } -> `Herr' #}
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

