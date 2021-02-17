module Data.HDF5.Types
  ( Object (..),
    ObjectType (..),
    File,
    Group,
    Dataset,
    Datatype,
    Handle (..),
    rawHandle,
    close,
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

import Control.Monad.ST (RealWorld)
import Control.Monad.Trans.Resource
import Data.Vector.Storable (MVector)
import Foreign.C.Types (CInt, CUInt)
import Prelude hiding (Handle)

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

data Handle = Handle {-# UNPACK #-} !Hid {-# UNPACK #-} !ReleaseKey

-- | A HDF5 object.
data Object (k :: ObjectType) where
  File :: {-# UNPACK #-} !Handle -> Object 'FileTy
  Group :: {-# UNPACK #-} !Handle -> Object 'GroupTy
  Dataset :: {-# UNPACK #-} !Handle -> Object 'DatasetTy
  Datatype :: {-# UNPACK #-} !Handle -> Object 'DatatypeTy

newtype Dataspace = Dataspace Handle

newtype Attribute = Attribute Handle

type File = Object 'FileTy

type Group = Object 'GroupTy

type Dataset = Object 'DatasetTy

type Datatype = Object 'DatatypeTy

class HasRawHandle a where
  rawHandle :: a -> Hid

instance HasRawHandle (Object t) where
  rawHandle x = case x of
    (File (Handle h _)) -> h
    (Group (Handle h _)) -> h
    (Dataset (Handle h _)) -> h
    (Datatype (Handle h _)) -> h

instance HasRawHandle Dataspace where
  rawHandle (Dataspace (Handle h _)) = h

instance HasRawHandle Attribute where
  rawHandle (Attribute (Handle h _)) = h

class CanClose a where
  close :: MonadResource m => a -> m ()

instance CanClose Dataspace where
  close (Dataspace (Handle _ k)) = release k

instance CanClose Attribute where
  close (Attribute (Handle _ k)) = release k

instance CanClose Group where
  close (Group (Handle _ k)) = release k

instance CanClose Dataset where
  close (Dataset (Handle _ k)) = release k

instance CanClose Datatype where
  close (Datatype (Handle _ k)) = release k

class KnownDatatype a where
  ofType :: MonadResource m => m Datatype

data ArrayView = ArrayView !Datatype !Dataspace (MVector RealWorld Word8)

class KnownDataset a where
  withArrayView :: (HasCallStack, MonadResource m) => a -> (ArrayView -> m b) -> m b
  peekArrayView :: (HasCallStack, MonadResource m) => ArrayView -> m a
