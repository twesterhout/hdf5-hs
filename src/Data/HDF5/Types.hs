module Data.HDF5.Types
  ( HDF5 (..)
  , runHDF5
  , createHandle
  , fromCString
  , ObjectType (..)
  , Handle (..)
  , Scalar (..)
  , Attribute (..)
  , ArrayView (..)
  , ElementOf

    -- * Low-level types

    -- | See https://github.com/JuliaIO/HDF5.jl/blob/master/src/api_types.jl
  , Haddr
  , Hbool
  , Herr
  , Hid
  , Hsize
  , Hssize
  , Htri
  , H5E_error2_t
  , H5O_info1_t
  , H5L_info_t
  , H5I_type_t (..)
  , H5S_seloper_t (..)
  )
where

import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Resource.Internal (ReleaseKey (..))
import Data.ByteString qualified as BS
import Data.Int
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Typeable
import Data.Word
import Foreign.C.String (CString)
import Foreign.C.Types (CInt, CUInt)
import GHC.ForeignPtr (ForeignPtr (..))
import GHC.Generics (Generic)
import GHC.Records (HasField (..))

type Haddr = Word64

type Hbool = CUInt

type Herr = CInt

type Hid = Int64

type Hsize = Word64

type Hssize = Int64

type Htri = CInt

data H5E_error2_t

data H5O_info1_t

data H5L_info_t

data H5I_type_t
  = H5I_FILE
  | H5I_GROUP
  | H5I_DATATYPE
  | H5I_DATASPACE
  | H5I_DATASET
  | H5I_ATTR
  deriving stock (Read, Show, Eq)

data H5S_seloper_t
  = H5S_SELECT_SET
  | H5S_SELECT_AND
  | H5S_SELECT_OR
  | H5S_SELECT_XOR
  | H5S_SELECT_NOTB
  | H5S_SELECT_NOTA
  deriving stock (Read, Show, Eq, Generic)
  deriving anyclass (NFData)

-- | A tag type for 'Object' GADT. Allows us to have polymorphic algorithms
-- while keeping everything type-safe.
data ObjectType = FileTy | GroupTy | DatasetTy | DatatypeTy
  deriving stock (Read, Show, Eq, Generic, Typeable)

data Handle s = Handle {-# UNPACK #-} !Hid {-# UNPACK #-} !ReleaseKey
  deriving stock (Generic, Typeable)

instance NFData (Handle s) where
  rnf (Handle a (ReleaseKey b c)) = a `deepseq` b `deepseq` c `deepseq` ()

newtype Scalar a = Scalar a
  deriving stock (Read, Show, Eq, Generic)
  deriving anyclass (NFData)

newtype Attribute s = Attribute (Handle s)

instance HasField "rawHandle" (Attribute s) Hid where
  getField (Attribute (Handle h _)) = h
  {-# INLINE getField #-}

-- class CanClose a where
--   close :: MonadResource m => a -> m ()
--
-- instance CanClose Dataspace where
--   close (Dataspace (Handle _ k)) = release k
--
-- instance CanClose Attribute where
--   close (Attribute (Handle _ k)) = release k
--
-- instance CanClose Group where
--   close (Group (Handle _ k)) = release k
--
-- instance CanClose Dataset where
--   close (Dataset (Handle _ k)) = release k
--
-- instance CanClose Datatype where
--   close (Datatype (Handle _ k)) = release k

data ArrayView a
  = ArrayView
      !(ForeignPtr a)
      -- ^ Data pointer
      ![Int]
      -- ^ Shape
      ![Int]
      -- ^ Strides
  deriving stock (Generic)

-- arrayViewFromPtrShape :: Ptr a -> [Int] -> (ArrayView a -> m b) -> m b
-- arrayViewFromPtrShape = undefined

-- arrayViewFromPtrShapeStrides :: Ptr a -> [Int] -> [Int] -> (ArrayView a -> m b) -> m b
-- arrayViewFromPtrShapeStrides = undefined

instance NFData (ArrayView a) where
  rnf (ArrayView (ForeignPtr !_ !_) shape stride) = shape `deepseq` stride `deepseq` ()

-- data DatasetSlice s = DatasetSlice !(Dataset s) !Hyperslab
--   deriving stock (Generic)
--   deriving anyclass (NFData)

-- data ArrayView = ArrayView !Datatype !Dataspace (MVector RealWorld Word8)

type family ElementOf a

-- class KnownDatatype (ElementOf a) => KnownDataset a where
--   withArrayView :: (HasCallStack, MonadResource m) => a -> (ArrayView (ElementOf a) -> m b) -> m b
--   fromArrayView :: (HasCallStack, MonadResource m) => ArrayView (ElementOf a) -> m a

-- class KnownDataset a where
--   withArrayView :: (HasCallStack, MonadResource m) => a -> (ArrayView -> m b) -> m b
--   peekArrayView :: (HasCallStack, MonadResource m) => ArrayView -> m a

newtype HDF5 s m a = HDF5 (ResourceT m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadTrans, MonadResource)

runHDF5 :: MonadUnliftIO m => (forall s. HDF5 s m a) -> m a
runHDF5 (HDF5 resource) = runResourceT resource

createHandle :: MonadUnliftIO m => (Hid -> IO ()) -> IO Hid -> HDF5 s m (Handle s)
createHandle cleanup acquire =
  allocate acquire cleanup >>= \case
    (k, v) -> pure $ Handle v k

fromCString :: CString -> IO Text
fromCString p = decodeUtf8 <$> BS.packCString p
