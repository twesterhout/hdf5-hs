{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.HDF5.Group
  ( Group
  , createGroup
  , open
  , exists
  , h5o_close
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.HDF5.Context
import Data.HDF5.File
import Data.HDF5.Object
import Data.HDF5.Types
import Data.Some
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Foreign.C.Types
import Foreign.Marshal hiding (void)
import GHC.Stack
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU

C.context (C.baseCtx <> C.bsCtx <> C.funCtx <> h5Ctx)
C.include "<hdf5.h>"

createGroup :: (MonadUnliftIO m) => Group s -> Text -> HDF5 s m (Group s)
createGroup ((.rawHandle) -> parent) (encodeUtf8 -> path) = do
  Group
    <$> createHandle
      h5o_close
      [CU.exp| hid_t { H5Gcreate(
        $(hid_t parent), $bs-cstr:path, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) } |]

-- | Close the object.
h5o_close :: (MonadIO m) => Hid -> m ()
h5o_close h = liftIO . void $ [CU.exp| herr_t { H5Oclose($(hid_t h)) } |]

open :: (HasCallStack, MonadUnliftIO m) => Group s -> Text -> HDF5 s m (Some (Object s))
open ((.rawHandle) -> parent) (encodeUtf8 -> path) = do
  handle <- createHandle h5o_close [CU.exp| hid_t { H5Oopen($(hid_t parent), $bs-cstr:path, H5P_DEFAULT) } |]
  objectFromHandle handle

objectFromHandle :: (HasCallStack, MonadUnliftIO m) => Handle s -> HDF5 s m (Some (Object s))
objectFromHandle handle =
  getObjectType handle >>= \case
    H5I_FILE -> pure . mkSome . File $ handle
    H5I_GROUP -> pure . mkSome . Group $ handle
    H5I_DATASET -> pure . mkSome . Dataset $ handle
    H5I_DATATYPE -> pure . mkSome . Datatype $ handle
    t -> error $ "wrong handle type: " <> show t <> "; expected an Object"

-- | Check whether a direct link exists.
linkExists :: (MonadUnliftIO m) => Group s -> Text -> HDF5 s m Bool
linkExists ((.rawHandle) -> object) (encodeUtf8 -> path) =
  toBool <$> liftIO [CU.exp| htri_t { H5Lexists($(hid_t object), $bs-cstr:path, H5P_DEFAULT) } |]

exists :: forall m s. (HasCallStack, MonadUnliftIO m) => Group s -> Text -> HDF5 s m Bool
exists parent path
  | T.null path = pure True
  | T.head path == '/' = getRoot parent >>= flip exists (T.tail path)
  | otherwise = existsHelper parent (T.split (== '/') path)
  where
    existsHelper :: Group s -> [Text] -> HDF5 s m Bool
    existsHelper _ [] = pure True
    existsHelper p (first : rest) = do
      linkExists p first >>= \case
        True -> do
          open p first >>= \case
            (Some g@(Group _)) -> existsHelper g rest
            _ -> pure $ null rest
        False -> pure False

-- | Get object type.
getObjectType :: (HasCallStack, MonadUnliftIO m) => Handle s -> HDF5 s m H5I_type_t
getObjectType (Handle h _) = do
  tp <- liftIO [CU.exp| int { H5Iget_type($(hid_t h)) } |]
  let r
        | toBool [CU.pure| bool { $(int tp) == H5I_FILE } |] = H5I_FILE
        | toBool [CU.pure| bool { $(int tp) == H5I_GROUP } |] = H5I_GROUP
        | toBool [CU.pure| bool { $(int tp) == H5I_DATATYPE } |] = H5I_DATATYPE
        | toBool [CU.pure| bool { $(int tp) == H5I_DATASPACE } |] = H5I_DATASPACE
        | toBool [CU.pure| bool { $(int tp) == H5I_DATASET } |] = H5I_DATASET
        | toBool [CU.pure| bool { $(int tp) == H5I_ATTR } |] = H5I_ATTR
        | otherwise = error $ "invalid H5I_type_t: " <> show tp
  pure r
