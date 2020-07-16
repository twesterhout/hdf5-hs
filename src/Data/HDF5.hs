{-# LANGUAGE NoImplicitPrelude #-}

module Data.HDF5
  ( h5Open,
    h5Close,
    h5Name,
    h5Size,
    h5OpenByIndex,
  )
where

import Data.HDF5.Internal
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Relude
import System.Directory (doesFileExist)

data H5Handle
  = H5File !Hid
  | H5Group !Hid
  deriving stock (Eq, Show)

unHandle :: H5Handle -> Hid
unHandle = \case
  H5File h -> h
  H5Group h -> h

-- h5Check :: (MonadIO m, MonadFail m) => Maybe String -> Hid -> m Hid
-- h5Check msg code
--   | code < 0 = fail $ fromMaybe ("failed with error code " <> show code) msg
--   | otherwise = return code

h5Open :: (MonadIO m, MonadFail m) => FilePath -> IOMode -> m H5Handle
h5Open path mode = do
  r <-
    liftIO $
      doesFileExist path >>= \case
        True ->
          let flags = if mode == ReadMode then H5F_ACC_RDONLY else H5F_ACC_RDWR
           in h5f_open path flags H5P_DEFAULT
        False -> do
          when (mode == ReadMode || mode == AppendMode) $
            fail $ "file " <> show path <> " not found"
          h5f_create path H5F_ACC_EXCL H5P_DEFAULT H5P_DEFAULT
  when (r < 0) $ fail ("error opening file: " <> show path <> "; error code: " <> show r)
  return $ H5File r

h5Close :: (MonadIO m, MonadFail m) => H5Handle -> m ()
h5Close = \case
  H5File handle -> do
    status <- liftIO $ h5f_close handle
    when (status < 0) $ fail "error closing file"
  H5Group handle -> do
    status <- liftIO $ h5o_close handle
    when (status < 0) $ fail "error closing group"

checkValidHandle :: (MonadFail m, MonadIO m) => H5Handle -> m ()
checkValidHandle handle
  | h < 0 = fail $ "handle is invalid: " <> show handle
  | otherwise =
    (liftIO $ h5i_is_valid h) >>= \status -> case compare status 0 of
      LT -> fail $ "H5Iis_valid failed with error code " <> show status
      EQ -> fail $ "handle is invalid: " <> show handle
      _ -> return ()
  where
    h = unHandle handle

h5Size :: (MonadIO m, MonadFail m) => H5Handle -> m Int
h5Size g =
  checkValidHandle g >> do
    r <- liftIO . h5g_get_num_objs . unHandle $ g
    when (r < 0) . fail $ "error getting group size; error code: " <> show r
    return r

h5Name :: (MonadIO m, MonadFail m) => H5Handle -> m String
h5Name handle =
  let h = unHandle handle
   in checkValidHandle handle >> do
        liftIO (h5i_get_name h nullPtr 0) >>= \r -> case compare r 0 of
          GT -> liftIO $
            allocaBytes (r + 1) $ \s -> do
              status <- h5i_get_name h s (r + 1)
              when (status < 0) $ failWithCode status
              peekCString s
          EQ -> return ""
          LT -> failWithCode r
  where
    failWithCode e = fail $ "error getting object's name; error code: " <> show e

h5OpenByIndex :: (MonadIO m, MonadFail m) => H5Handle -> Int -> m H5Handle
h5OpenByIndex parent index =
  checkValidHandle parent >> do
    groupSize <- h5Size parent
    when (index < 0 || index >= groupSize) . fail $ "invalid index: " <> show index
    r <- liftIO $ h5o_open_by_idx (unHandle parent) "." H5_INDEX_NAME H5_ITER_INC index H5P_DEFAULT
    when (r < 0) . fail $ "error accessing object at index " <> show index
    return $ H5Group r
