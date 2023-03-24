{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.HDF5.Datatype
  ( Datatype
  , KnownDatatype (..)
  , getDatatypeSize
  )
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.ByteString qualified as BS
import Data.HDF5.Context
import Data.HDF5.Group
import Data.HDF5.Object
import Data.HDF5.Types
import Data.Int
import Data.Text (Text)
import Data.Word
import Foreign.C.Types
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU

C.context (C.baseCtx <> C.bsCtx <> C.funCtx <> h5Ctx)
C.include "<hdf5.h>"

class KnownDatatype a where
  createDatatype :: MonadUnliftIO m => HDF5 s m (Datatype s)

createStaticDatatype :: MonadUnliftIO m => Hid -> HDF5 s m (Datatype s)
createStaticDatatype p = Datatype . Handle p <$> register (pure ())

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

instance KnownDatatype Int32 where createDatatype = createStaticDatatype h5t_NATIVE_INT32

instance KnownDatatype Int64 where createDatatype = createStaticDatatype h5t_NATIVE_INT64

instance KnownDatatype Int where createDatatype = createDatatype @Int64

instance KnownDatatype Word32 where createDatatype = createStaticDatatype h5t_NATIVE_UINT32

instance KnownDatatype Word64 where createDatatype = createStaticDatatype h5t_NATIVE_UINT64

instance KnownDatatype Word where createDatatype = createDatatype @Word64

instance KnownDatatype CFloat where createDatatype = createStaticDatatype h5t_NATIVE_FLOAT

instance KnownDatatype Float where createDatatype = createDatatype @CFloat

instance KnownDatatype CDouble where createDatatype = createStaticDatatype h5t_NATIVE_DOUBLE

instance KnownDatatype Double where createDatatype = createDatatype @CDouble

getTextDatatype :: (MonadUnliftIO m) => HDF5 s m (Datatype s)
getTextDatatype =
  Datatype
    <$> createHandle
      h5o_close
      [CU.block| hid_t {
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

instance KnownDatatype Text where createDatatype = getTextDatatype

instance KnownDatatype String where createDatatype = getTextDatatype

instance KnownDatatype BS.ByteString where createDatatype = getTextDatatype

getDatatypeSize :: (MonadUnliftIO m) => Datatype s -> HDF5 s m Int
getDatatypeSize ((.rawHandle) -> h) =
  fromIntegral
    <$> liftIO
      [C.block| int64_t {
        hsize_t const size = H5Tget_size($(hid_t h));
        return size == 0 ? (-1) : (int64_t)size;
      } |]
