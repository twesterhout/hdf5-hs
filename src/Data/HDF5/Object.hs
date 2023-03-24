{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.HDF5.Object
  ( Object (..)
  , File
  , Group
  , Dataset
  , Datatype
  )
where

import Control.DeepSeq (NFData (..), deepseq, force)
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Resource.Internal (ReleaseKey (..))
import Data.HDF5.Context
import Data.HDF5.Types
import Data.Int
import Data.Some
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as V
import Data.Vector.Storable.Mutable qualified as MV
import Data.Word
import Foreign.C.Types
import Foreign.Marshal hiding (void)
import Foreign.Ptr (Ptr, nullPtr)
import GHC.ForeignPtr (ForeignPtr (..))
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import GHC.Stack
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)

C.context (C.baseCtx <> C.bsCtx <> C.funCtx <> h5Ctx)
C.include "<hdf5.h>"
C.include "<hdf5_hl.h>"

-- | A HDF5 object.
data Object s (k :: ObjectType) where
  File :: {-# UNPACK #-} !(Handle s) -> Object s 'FileTy
  Group :: {-# UNPACK #-} !(Handle s) -> Object s 'GroupTy
  Dataset :: {-# UNPACK #-} !(Handle s) -> Object s 'DatasetTy
  Datatype :: {-# UNPACK #-} !(Handle s) -> Object s 'DatatypeTy

deriving stock instance Typeable (Object k)

instance NFData (Object s k) where
  rnf (File h) = rnf h
  rnf (Group h) = rnf h
  rnf (Dataset h) = rnf h
  rnf (Datatype h) = rnf h

instance HasField "rawHandle" (Object s k) Hid where
  getField x = case x of
    File (Handle h _) -> h
    Group (Handle h _) -> h
    Dataset (Handle h _) -> h
    Datatype (Handle h _) -> h
  {-# INLINE getField #-}

type File s = Object s 'FileTy

type Group s = Object s 'GroupTy

type Dataset s = Object s 'DatasetTy

type Datatype s = Object s 'DatatypeTy

instance Eq (Datatype s) where
  ((.rawHandle) -> a) == ((.rawHandle) -> b) =
    unsafePerformIO . fmap force $
      fromHtri =<< [CU.exp| htri_t { H5Tequal($(hid_t a), $(hid_t b)) } |]

instance Show (Datatype s) where
  show ((.rawHandle) -> h) = unsafePerformIO . fmap (unpack . force) $ do
    numBytes <- c_dtype_to_text h nullPtr 0
    allocaBytes (numBytes + 1) $ \s ->
      c_dtype_to_text h s (numBytes + 1) >> fromCString s
    where
      c_dtype_to_text :: Hid -> Ptr CChar -> Int -> IO Int
      c_dtype_to_text x buffer (fromIntegral -> size) =
        fromIntegral
          <$> [CU.block| hssize_t {
                size_t len = $(size_t size);
                herr_t status = H5LTdtype_to_text($(hid_t x), $(char* buffer), H5LT_DDL, &len);
                return (status < 0) ? status : (hssize_t)len;
              } |]
