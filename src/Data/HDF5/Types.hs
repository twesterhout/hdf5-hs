module Data.HDF5.Types
  ( Object (..),
    ObjectType (..),
    File,
    Group,
    Dataset,
    Datatype,
    Dataspace (..),
    Attribute (..),
    ArrayView (..),
    KnownDatatype (..),
    KnownDataset (..),

    -- * Low-level types

    -- | See https://github.com/JuliaIO/HDF5.jl/blob/master/src/api_types.jl
    Haddr,
    Hbool,
    Herr,
    Hid,
    Hsize,
    Hssize,
    Htri,
    H5E_error2_t,
    H5O_info1_t,
    H5L_info_t,
    H5I_type_t (..),
  )
where

import Control.Exception.Safe
import Control.Monad.ST (RealWorld)
import Data.Constraint (Dict (..))
import Data.Vector.Storable (MVector)
import Foreign.C.Types (CInt, CUInt)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable

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

-- | A tag type for 'Object' GADT. Allows us to have polymorphic algorithms
-- while keeping everything type-safe.
data ObjectType = FileTy | GroupTy | DatasetTy | DatatypeTy
  deriving stock (Read, Show, Eq)

-- | A HDF5 object.
data Object (k :: ObjectType) where
  File :: {-# UNPACK #-} !Hid -> Object 'FileTy
  Group :: {-# UNPACK #-} !Hid -> Object 'GroupTy
  Dataset :: {-# UNPACK #-} !Hid -> Object 'DatasetTy
  Datatype :: {-# UNPACK #-} !Hid -> Object 'DatatypeTy

newtype Dataspace = Dataspace {unDataspace :: Hid}

newtype Attribute = Attribute {unAttribute :: Hid}

type File = Object 'FileTy

type Group = Object 'GroupTy

type Dataset = Object 'DatasetTy

type Datatype = Object 'DatatypeTy

class MkObject (t :: ObjectType) where
  unsafeMkObject :: Hid -> Object t

instance MkObject 'FileTy where unsafeMkObject = File

instance MkObject 'GroupTy where unsafeMkObject = Group

instance MkObject 'DatasetTy where unsafeMkObject = Dataset

instance MkObject 'DatatypeTy where unsafeMkObject = Datatype

peekStorable :: forall a. Storable a => Ptr () -> Int -> IO a
peekStorable p n
  | n == objectSize = peek $ castPtr p
  | otherwise =
    error $
      "invalid buffer size: " <> show n <> "; expected " <> show objectSize
  where
    objectSize :: Int
    objectSize = sizeOf dummy
    dummy :: a
    dummy = dummy

withStorable :: Storable a => a -> (Ptr () -> Int -> IO b) -> IO b
withStorable x func = with x $ \buffer -> func (castPtr buffer) (sizeOf x)

class KnownDatatype a where
  withDatatype :: (MonadIO m, MonadMask m) => proxy a -> (Datatype -> m b) -> m b

  hasStorable :: proxy a -> Maybe (Dict (Storable a))
  hasStorable _ = Nothing

  h5Peek :: Ptr () -> Int -> IO a
  default h5Peek :: Storable a => Ptr () -> Int -> IO a
  h5Peek = peekStorable

  h5With :: a -> (Ptr () -> Int -> IO b) -> IO b
  default h5With :: Storable a => a -> (Ptr () -> Int -> IO b) -> IO b
  h5With = withStorable

data ArrayView = ArrayView !Datatype !Dataspace (MVector RealWorld Word8)

class KnownDataset a where
  withArrayView :: HasCallStack => a -> (ArrayView -> IO b) -> IO b
  peekArrayView :: HasCallStack => ArrayView -> IO a
