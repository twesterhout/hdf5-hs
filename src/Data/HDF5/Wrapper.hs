{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.HDF5.Wrapper
  ( -- openFile,
    -- closeFile,
    -- withFile,
    h5f_open,
    h5f_create,
    h5f_close,
    h5f_get_filesize,
    h5o_close,
    h5g_get_num_objs,
    h5i_get_name,
    h5o_open,
    h5o_open_by_idx,
    h5l_iterate,
    h5l_delete,
    h5i_get_file_id,
    h5i_get_type,
    h5l_exists,
    exists,
    h5g_create,
    OpenAccessFlag (..),
    CreateAccessFlag (..),
  )
where

import Control.Exception.Safe
import qualified Data.ByteString as B
import Data.HDF5.Context
import Data.HDF5.Types
import qualified Data.Text as T
import Foreign.C.String (CString)
import Foreign.C.Types (CChar, CUInt)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Utils (toBool)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr, nullPtr)
import Foreign.Storable
import qualified GHC.Show
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import Prelude hiding (first, group)

C.context (C.baseCtx <> C.bsCtx <> C.funCtx <> h5Ctx)
C.include "<hdf5.h>"

class CheckError' a b | a -> b where
  checkError' :: HasCallStack => a -> b

instance CheckError' Hid Hid where
  checkError' x
    | x < 0 = error $ "HDF5 failed with error code " <> show x
    | otherwise = x

instance CheckError' Herr () where
  checkError' x
    | x < 0 = error $ "HDF5 failed with error code " <> show x
    | otherwise = ()

data ErrorInfo = ErrorInfo !Text !Text !Int !Text

fromCString :: CString -> IO Text
fromCString p = decodeUtf8 <$> B.packCString p

-- @
-- typedef struct H5E_error2_t {
--     hid_t       cls_id;    /*class ID                           */
--     hid_t       maj_num;   /*major error ID		     */
--     hid_t       min_num;   /*minor error number		     */
--     unsigned    line;      /*line in file where error occurs    */
--     const char *func_name; /*function in which error occurred   */
--     const char *file_name; /*file in which error occurred       */
--     const char *desc;      /*optional supplied description      */
-- } H5E_error2_t;
-- @
peekErrorInfo :: Ptr H5E_error2_t -> IO ErrorInfo
peekErrorInfo p =
  ErrorInfo
    <$> (fromCString =<< [C.exp| const char* { $(H5E_error2_t* p)->file_name } |])
    <*> (fromCString =<< [C.exp| const char* { $(H5E_error2_t* p)->func_name } |])
    <*> (fromIntegral <$> [C.exp| unsigned int { $(H5E_error2_t* p)->line } |])
    <*> (fromCString =<< [C.exp| const char* { $(H5E_error2_t* p)->desc } |])

prettyErrorInfo :: ErrorInfo -> Text
prettyErrorInfo (ErrorInfo file func line msg) =
  file <> ":" <> show line <> ":" <> func <> "(): " <> msg

type H5E_walk2_t = CUInt -> Ptr H5E_error2_t -> Ptr () -> IO Herr

foreign import ccall "wrapper"
  mkWalk :: H5E_walk2_t -> IO (FunPtr H5E_walk2_t)

collectStack :: HasCallStack => IO [ErrorInfo]
collectStack = uninterruptibleMask_ $
  bracket h5e_get_current_stack h5e_close_stack $ \stackId -> do
    (listRef :: IORef [ErrorInfo]) <- newIORef []
    let callback _ ptr _ = peekErrorInfo ptr >>= \info -> modifyIORef listRef ((:) info) >> return 0
    bracket (mkWalk callback) freeHaskellFunPtr $ \fn -> do
      h5e_walk2 stackId fn
    readIORef listRef
  where
    h5e_get_current_stack :: HasCallStack => IO Hid
    h5e_get_current_stack = checkError' <$> [C.exp| hid_t { H5Eget_current_stack() } |]
    h5e_close_stack :: HasCallStack => Hid -> IO ()
    h5e_close_stack x = checkError' <$> [C.exp| herr_t { H5Eclose_stack($(hid_t x)) } |]
    h5e_walk2 :: HasCallStack => Hid -> FunPtr H5E_walk2_t -> IO ()
    h5e_walk2 stack func =
      fmap checkError' $
        [C.exp| herr_t {
          H5Ewalk2($(hid_t stack),
                   H5E_WALK_UPWARD,
                   $(herr_t (* func)(unsigned int, const H5E_error2_t*, void*)),
                   NULL) } |]

data H5Exception = H5Exception !Int ![ErrorInfo] !(Maybe Text)
  deriving stock (Generic)

instance Exception H5Exception

instance Show H5Exception where
  show = toString . prettyH5Exception

prettyH5Exception :: H5Exception -> Text
prettyH5Exception (H5Exception code stack msg) = "HDF5 error " <> show code <> msg' <> stack'
  where
    msg' = maybe "" (": " <>) msg
    stack' = case (intersperse "\n  " $ prettyErrorInfo <$> stack) of
      xs@(_ : _) -> mconcat $ "\n  " : xs
      [] -> ""

_checkError :: Integral a => Maybe Text -> a -> IO a
_checkError msg x
  | x < 0 = collectStack >>= \stack -> throw $ H5Exception (fromIntegral x) stack msg
  | otherwise = return x

checkErrorWithMsg :: Integral a => Text -> a -> IO a
checkErrorWithMsg msg = _checkError (Just msg)

checkError :: Integral a => a -> IO a
checkError = _checkError Nothing

data OpenAccessFlag
  = ReadOnly
  | ReadWrite
  | SWMRRead
  | SWMRWrite
  deriving stock (Read, Show, Eq)

openAccessFlagToUInt :: OpenAccessFlag -> CUInt
openAccessFlagToUInt ReadOnly = [CU.pure| unsigned int { H5F_ACC_RDONLY } |]
openAccessFlagToUInt ReadWrite = [CU.pure| unsigned int { H5F_ACC_RDWR } |]
openAccessFlagToUInt SWMRRead = [CU.pure| unsigned int { H5F_ACC_RDONLY | H5F_ACC_SWMR_READ } |]
openAccessFlagToUInt SWMRWrite = [CU.pure| unsigned int { H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE } |]

data CreateAccessFlag
  = Truncate
  | Exclusive
  deriving stock (Read, Show, Eq)

createAccessFlagToUInt :: CreateAccessFlag -> CUInt
createAccessFlagToUInt Truncate = [CU.pure| unsigned int { H5F_ACC_TRUNC } |]
createAccessFlagToUInt Exclusive = [CU.pure| unsigned int { H5F_ACC_EXCL } |]

h5f_open :: Text -> OpenAccessFlag -> IO Hid
h5f_open filename flags = do
  let c_filename = encodeUtf8 filename
      c_flags = openAccessFlagToUInt flags
  checkError =<< [C.exp| hid_t { H5Fopen($bs-ptr:c_filename, $(unsigned int c_flags), H5P_DEFAULT) } |]

h5f_create :: Text -> CreateAccessFlag -> IO Hid
h5f_create filename flags = do
  let c_filename = encodeUtf8 filename
      c_flags = createAccessFlagToUInt flags
  checkError =<< [C.exp| hid_t { H5Fcreate($bs-ptr:c_filename, $(unsigned int c_flags), H5P_DEFAULT, H5P_DEFAULT) } |]

h5f_close :: Hid -> IO ()
h5f_close file = void . checkError =<< [C.exp| herr_t { H5Fclose($(hid_t file)) } |]

h5f_get_filesize :: Hid -> IO ()
h5f_get_filesize file =
  void . checkError
    =<< [C.block| int64_t {
          hsize_t size;
          herr_t status = H5Fget_filesize($(hid_t file), &size);
          return status < 0 ? (int64_t)status : (int64_t)size;
        } |]

h5o_close :: Hid -> IO ()
h5o_close h = void . checkError =<< [C.exp| herr_t { H5Oclose($(hid_t h)) } |]

h5g_get_num_objs :: Hid -> IO Int
h5g_get_num_objs h =
  fmap fromIntegral . checkError
    =<< [C.block| int64_t {
          H5G_info_t group_info;
          herr_t status = H5Gget_info($(hid_t h), &group_info);
          return status < 0 ? (int64_t)status : (int64_t)group_info.nlinks;
        } |]

h5i_get_name :: Hid -> IO Text
h5i_get_name h = do
  numBytes <- c_get_name h nullPtr 0
  allocaBytes (numBytes + 1) $ \s -> c_get_name h s (numBytes + 1) >> fromCString s
  where
    c_get_name :: Hid -> Ptr CChar -> Int -> IO Int
    c_get_name x buffer size =
      let c_size = fromIntegral size
       in fmap fromIntegral . checkError
            =<< [C.exp| hssize_t { H5Iget_name($(hid_t x), $(char* buffer), $(size_t c_size)) } |]

h5o_open :: Hid -> Text -> IO Hid
h5o_open parent path = checkError =<< [C.exp| hid_t { H5Oopen($(hid_t parent), $bs-ptr:c_path, H5P_DEFAULT) } |]
  where
    c_path = encodeUtf8 path

h5o_open_by_idx :: HasCallStack => Hid -> Int -> IO Hid
h5o_open_by_idx parent index
  | index < 0 = error $ "invalid index: " <> show index
  | otherwise =
    checkError
      =<< [C.exp| hid_t { H5Oopen_by_idx($(hid_t parent), ".", H5_INDEX_NAME,
                                         H5_ITER_INC, $(hsize_t c_index), H5P_DEFAULT) } |]
  where
    c_index = fromIntegral index

type H5L_iterate_t = Hid -> Ptr CChar -> Ptr H5L_info_t -> Ptr () -> IO Herr

foreign import ccall "wrapper"
  mkH5L_iterate_t :: H5L_iterate_t -> IO (FunPtr H5L_iterate_t)

h5l_iterate :: Hid -> (Hid -> Text -> IO ()) -> IO ()
h5l_iterate group action = do
  let helper h c_name _ _ = fromCString c_name >>= action h >> return 0
  bracket (mkH5L_iterate_t helper) freeHaskellFunPtr $ \actionPtr ->
    void . checkError
      =<< [C.exp| herr_t {
          H5Literate($(hid_t group),
                     H5_INDEX_NAME,
                     H5_ITER_INC,
                     NULL,
                     $(herr_t (* actionPtr)(hid_t, const char*, const H5L_info_t*, void*)),
                     NULL)
          } |]

h5l_delete :: Hid -> Text -> IO ()
h5l_delete parent path =
  void . checkError =<< [C.exp| herr_t { H5Ldelete($(hid_t parent), $bs-ptr:c_path, H5P_DEFAULT) } |]
  where
    c_path = encodeUtf8 path

instance Enum H5I_type_t where
  fromEnum x = fromIntegral $ case x of
    H5I_FILE -> [CU.pure| int { H5I_FILE } |]
    H5I_GROUP -> [CU.pure| int { H5I_GROUP } |]
    H5I_DATATYPE -> [CU.pure| int { H5I_DATATYPE } |]
    H5I_DATASPACE -> [CU.pure| int { H5I_DATASPACE } |]
    H5I_DATASET -> [CU.pure| int { H5I_DATASET } |]
    H5I_ATTR -> [CU.pure| int { H5I_ATTR } |]
  toEnum x
    | toBool [CU.pure| bool { $(int c_x) == H5I_FILE } |] = H5I_FILE
    | toBool [CU.pure| bool { $(int c_x) == H5I_GROUP } |] = H5I_GROUP
    | toBool [CU.pure| bool { $(int c_x) == H5I_DATATYPE } |] = H5I_DATATYPE
    | toBool [CU.pure| bool { $(int c_x) == H5I_DATASPACE } |] = H5I_DATASPACE
    | toBool [CU.pure| bool { $(int c_x) == H5I_DATASET } |] = H5I_DATASET
    | toBool [CU.pure| bool { $(int c_x) == H5I_ATTR } |] = H5I_ATTR
    | otherwise = error $ "invalid H5I_type_t: " <> show x
    where
      c_x = fromIntegral x

h5i_get_type :: Hid -> IO H5I_type_t
h5i_get_type h = do
  fmap (toEnum . fromIntegral) $
    checkError
      =<< [C.block| int {
            const H5I_type_t t = H5Iget_type($(hid_t h));
            return t == H5I_BADID ? -1 : t; } |]

h5i_get_file_id :: Hid -> IO Hid
h5i_get_file_id h = checkError =<< [C.exp| hid_t { H5Iget_file_id($(hid_t h)) } |]

h5l_exists :: Hid -> Text -> IO Bool
h5l_exists parent path = toBool <$> [C.exp| htri_t { H5Lexists($(hid_t parent), $bs-ptr:c_path, H5P_DEFAULT) } |]
  where
    c_path = encodeUtf8 path

exists :: Hid -> Text -> IO Bool
exists parent path
  | T.null path = return True
  | T.head path == '/' = bracket (h5i_get_file_id parent) h5o_close $ \g -> exists g (T.tail path)
  | otherwise = existsHelper parent (T.split (== '/') path)
  where
    existsHelper :: Hid -> [Text] -> IO Bool
    existsHelper _ [] = return True
    existsHelper p (first : rest) = do
      h5l_exists p first >>= \case
        True -> bracket (h5o_open p first) h5o_close $ \o ->
          h5i_get_type o >>= \case
            H5I_GROUP -> existsHelper o rest
            _ -> return $ null rest
        False -> return False

h5g_create :: Hid -> Text -> IO ()
h5g_create parent path =
  void . checkError
    =<< [C.block| herr_t {
          const hid_t g = H5Gcreate($(hid_t parent), $bs-ptr:c_path,
                                    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
          if (g < 0) { return (herr_t)g; }
          return H5Gclose(g); } |]
  where
    c_path = encodeUtf8 path
