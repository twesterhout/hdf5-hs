{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Copyright: (c) 2020-2021 Tom Westerhout
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Tom Westerhout <14264576+twesterhout@users.noreply.github.com>
module Data.HDF5.Context
  ( h5Ctx
  , fromHtri
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.HDF5.Types
import Data.Map qualified as Map
import GHC.Stack
import Language.C.Inline.Context (Context (..))
import Language.C.Types qualified as Types
import Language.Haskell.TH qualified as TH

-- See https://github.com/JuliaIO/HDF5.jl/blob/master/src/api_types.jl
h5TypesTable :: Map.Map Types.TypeSpecifier TH.TypeQ
h5TypesTable =
  Map.fromList
    [ (Types.TypeName "haddr_t", [t|Haddr|])
    , (Types.TypeName "hbool_t", [t|Hbool|])
    , (Types.TypeName "herr_t", [t|Herr|])
    , (Types.TypeName "hid_t", [t|Hid|])
    , (Types.TypeName "hsize_t", [t|Hsize|])
    , (Types.TypeName "hssize_t", [t|Hssize|])
    , (Types.TypeName "htri_t", [t|Htri|])
    , (Types.TypeName "H5E_error2_t", [t|H5E_error2_t|])
    , (Types.TypeName "H5O_info1_t", [t|H5O_info1_t|])
    , (Types.TypeName "H5L_info_t", [t|H5L_info_t|])
    ]

-- | Provides type mappings for better interoperability with "Language.C.Inline".
h5Ctx :: Context
h5Ctx = mempty {ctxTypesTable = h5TypesTable}

fromHtri :: (HasCallStack, MonadIO m) => Htri -> m Bool
fromHtri = pure . (> 0)
