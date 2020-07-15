module Data.HDF5
  ( h5Open,
    h5Close,
  )
where

import Data.HDF5.Internal
import System.Directory (doesFileExist)

-- import System.IO (IOMode (..))

newtype H5Handle = H5Handle {unH5Handle :: Hid}
  deriving stock (Show)
  deriving newtype (Eq)

h5Check :: (MonadIO m, MonadFail m) => Maybe String -> Hid -> m Hid
h5Check msg code
  | code < 0 = fail $ fromMaybe ("failed with error code " <> show code) msg
  | otherwise = return code

h5Open :: (MonadIO m, MonadFail m) => FilePath -> IOMode -> m H5Handle
h5Open path mode = do
  r <-
    liftIO $
      doesFileExist path >>= \case
        True ->
          let flags = if mode == ReadMode then H5F_ACC_RDONLY else H5F_ACC_RDWR
           in h5_open path flags h5_DEFAULT
        False -> h5_create path H5F_ACC_EXCL h5_DEFAULT h5_DEFAULT
  when (r < 0) $ fail ("error opening file: " <> show path <> "; error code: " <> show r)
  return (H5Handle r)

h5Close :: (MonadIO m, MonadFail m) => H5Handle -> m ()
h5Close handle = do
  status <- liftIO $ h5_close (unH5Handle handle)
  when (status < 0) $ fail "error closing file"
