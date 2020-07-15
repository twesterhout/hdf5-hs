{-# LANGUAGE CPP #-}

module Data.HDF5.Internal where

-- import Foreign.C.Types (CUInt)

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
{#typedef hsize_t Hsize#}
{#typedef hssize_t Hssize#}
{#typedef htri_t Htri#}
{#typedef hbool_t Hbool#}

h5_DEFAULT :: Hid
h5_DEFAULT = 0

data H5F_ACC = H5F_ACC_RDONLY
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
    H5F_ACC_RDONLY     -> 0x0000
    H5F_ACC_RDWR       -> 0x0001
    H5F_ACC_TRUNC      -> 0x0002
    H5F_ACC_EXCL       -> 0x0004
    H5F_ACC_DEBUG      -> 0x0008
    H5F_ACC_CREAT      -> 0x0010
    H5F_ACC_SWMR_WRITE -> 0x0020
    H5F_ACC_SWMR_READ  -> 0x0040


-- hid_t H5Fopen( const char *name, unsigned flags, hid_t fapl_id )
{#fun H5Fopen as h5_open { `String', withEnum `H5F_ACC', `Hid' } -> `Hid' #}
  where withEnum = fromIntegral . fromEnum
-- hid_t H5Fcreate( const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id )
{#fun H5Fcreate as h5_create { `String', withEnum `H5F_ACC', `Hid', `Hid' } -> `Hid' #}
  where withEnum = fromIntegral . fromEnum
-- herr_t H5Fclose( hid_t file_id )
{#fun H5Fclose as h5_close { `Hid' } -> `Herr' #}

