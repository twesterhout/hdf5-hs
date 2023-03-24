{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.HDF5.File
  ( File
  , withFile
  , getRoot
  , getFile
  , AccessFlags (..)
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.HDF5.Context
import Data.HDF5.Object
import Data.HDF5.Types
import Data.Text
import Data.Text.Encoding
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as V
import Data.Vector.Storable.Mutable qualified as MV
import Foreign.C.Types
import GHC.Generics (Generic)
import GHC.Stack
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import System.Directory (doesFileExist)

C.context (C.baseCtx <> C.bsCtx <> C.funCtx <> h5Ctx)
C.include "<hdf5.h>"

data AccessFlags
  = ReadOnly
  | WriteTruncate
  | WriteAppend
  deriving stock (Read, Show, Eq)

withFile
  :: (HasCallStack, MonadUnliftIO m)
  => Text
  -- ^ filename
  -> AccessFlags
  -- ^ mode in which to open the file
  -> (forall s. Group s -> HDF5 s m a)
  -- ^ action to perform
  -> m a
withFile path flags action = runHDF5 $ openFile path flags >>= getRoot >>= action

openFile
  :: (HasCallStack, MonadUnliftIO m)
  => Text
  -- ^ filename
  -> AccessFlags
  -- ^ mode in which to open the file
  -> HDF5 s m (File s)
  -- ^ file handle
openFile filename@(encodeUtf8 -> c_filename) flags@(accessFlagsToUInt -> c_flags) =
  fmap File $
    case flags of
      ReadOnly -> open
      WriteTruncate -> create
      WriteAppend ->
        liftIO (doesFileExist (unpack filename)) >>= \case
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
h5f_close h = void [CU.exp| herr_t { H5Fclose($(hid_t h)) } |]

-- | Get the file in which the object is stored.
getFile :: (HasCallStack, MonadUnliftIO m) => Object s t -> HDF5 s m (File s)
getFile ((.rawHandle) -> h) =
  File
    <$> createHandle h5f_close [CU.exp| hid_t { H5Iget_file_id($(hid_t h)) } |]

-- | Get the group corresponding to the root of the file.
getRoot :: (HasCallStack, MonadUnliftIO m) => Object s t -> HDF5 s m (Group s)
getRoot object =
  getFile object >>= \case
    File h -> pure $ Group h

accessFlagsToUInt :: AccessFlags -> CUInt
accessFlagsToUInt ReadOnly = [CU.pure| unsigned int { H5F_ACC_RDONLY } |]
accessFlagsToUInt WriteTruncate = [CU.pure| unsigned int { H5F_ACC_TRUNC } |]
accessFlagsToUInt WriteAppend = [CU.pure| unsigned int { H5F_ACC_RDWR } |]
