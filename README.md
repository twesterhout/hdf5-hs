# hdf5-hs [![GitHub CI](https://github.com/twesterhout/hdf5-hs/workflows/CI/badge.svg)](https://github.com/twesterhout/hdf5-hs/actions)[![BSD-3-Clause license](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

> **Warning:** this package is a work-in-progress. The code is kind of working,
> but documentation and examples are lacking. Also this project was created
> specifically for [SpinED](...) program, so it's going to be "battle-tested"
> rather that a proof-of-concept.

High-level interface to [HDF5](https://www.hdfgroup.org/solutions/hdf5) for
Haskell. The idea is to provide a limited functionality, but in a user-friendly
way. It relies extensively on GADTs to keep the interface polymorphic, but
type-safe.


## Installation

This package is very much a work-in-progress and is not (yet, hopefully)
available on [Hackage](https://hackage.haskell.org/). Currently, the easiest way to install it is to use `cabal.project` and [specify that you want to install `hdf5-hs` from Git](https://cabal.readthedocs.io/en/3.4/cabal-project.html#specifying-packages-from-remote-version-control-locations):

```
source-repository-package
    type: git
    location: https://github.com/twesterhout/hdf5-hs.git
```


## Simple example

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.HDF5 (Dataset, Group, IOMode (..))
import qualified Data.HDF5 as H5
import Data.Text (Text)

main :: IO ()
main = do
  -- Create a file
  H5.withFile "example00.h5" WriteMode $ \file -> do
    -- Create a group
    H5.makeGroup file "myGroup"
    -- Do something with the group
    -- (matchM function "proves" to GHC that the object we've just opened is indeed a group)
    H5.byName file "myGroup" . H5.matchM @Group $ \group -> do
      -- Create a dataset
      H5.writeDataset group "myDataset" [(1 :: Int) .. 25]
      -- Add a description
      H5.byName group "myDataset" . H5.matchM @Dataset $ \dataset ->
        H5.writeAttribute dataset "myAttribute" ("Contains very important data 😀" :: Text)
```


### Related projects

There is [`hs-hdf5`](https://github.com/mokus0/hs-hdf5). It covers more
functionality of HDF5 than this package, but the API is lower-level.
