module Data.HDF5.Types
  ( Object (..),
    ObjectType (..),
    File,
    Group,
    Dataset,
    Datatype,

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

import Foreign.C.Types (CInt, CUInt)

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
  File :: Hid -> Object 'FileTy
  Group :: Hid -> Object 'GroupTy
  Dataset :: Hid -> Object 'DatasetTy
  Datatype :: Hid -> Object 'DatatypeTy

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
