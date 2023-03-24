-- |
-- Copyright: (c) 2020-2023 Tom Westerhout
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Tom Westerhout <14264576+twesterhout@users.noreply.github.com>
--
-- [HDF5](https://www.hdfgroup.org/solutions/hdf5) is a file format commonly
-- used for scientific data. It is especially great for storing large datasets
-- with lots of arrays or other structured data. This package provides a
-- high-level interface to [HDF5
-- library](https://portal.hdfgroup.org/pages/viewpage.action?pageId=50073943)
-- for Haskell programming language.
module Data.HDF5
  ( withFile
  , HDF5
  , AccessFlags (..)
  , File
  , Group
  , createGroup
  , open
  , exists
  , Dataset
  , KnownDataset (..)
  , Allocatable (..)
  , Scalar (..)
  , createDataset
  , writeDataset
  , writeDatasetExplicit
  , readDataset
  , readDatasetInto
  , readDatasetExplicit
  , getDatatype
  , getDataspace
  , createUnfilledDataset
  , Datatype
  , KnownDatatype (..)
  , getDatatypeSize
  , Dataspace
  , createDataspace
  , getDataspaceShape
  , isScalarDataspace
  , getSelection
  , Selection (..)
  , SelectionType (..)
  , Hyperslab (..)
  , selectHyperslab
  , selectHyperslabWith
  , Object (..)
  , runHDF5
  , H5S_seloper_t (..)
  , hyperslabBoundingBox
  , rowMajorStrides
  , colMajorStrides
  , MonadUnliftIO
  , Some (..)
  )
where

import Control.Monad.IO.Unlift
import Data.HDF5.Dataset
import Data.HDF5.Dataspace
import Data.HDF5.Datatype
import Data.HDF5.File
import Data.HDF5.Group
import Data.HDF5.Object
import Data.HDF5.Types
import Data.Some
