{-# LANGUAGE OverloadedStrings #-}

import qualified Data.HDF5 as H5
import Data.Vector.Storable (Vector)

{- Reproduces "Read and write to a dataset" example from HDF5 documentation:
   <https://raw.githubusercontent.com/HDFGroup/hdf5/develop/examples/h5_rdwt.c>
-}

main :: IO ()
main =
  -- Open an existing file
  H5.withFile "dset.h5" H5.WriteAppend $ \file -> do
    -- Write to dataset
    H5.createDataset file "/dset1" $
      [ [(1 :: Int) .. 6],
        [7 .. 12],
        [13 .. 18],
        [19 .. 24]
      ]
    H5.createDataset file "/dset2" $ [1.0 .. (6.0 :: Float)]

    -- Read from a dataset
    (xs :: [[Int]]) <- H5.open file "/dset1" >>= H5.readDataset
    print xs

    H5.createDataset file "/dset2" $ [1.0 .. (6.0 :: Float)]
    -- Read part of a dataset
    H5.open file "/dset2"
      >>= (H5.sliceDataset 0 2 3 1 >>> H5.readSelected @(Vector Float))
      >>= print
    -- prints [3.0,4.0,5.0]

    -- Update an existing dataset
    H5.open file "/dset2" >>= H5.writeDataset [(10 :: Float) .. 15]

    dataset <- H5.open file "/dset2"
    print =<< H5.readDataset @[Float] dataset
    -- Close the dataset early. Not strictly necessary, because ResourceT monad
    -- transformer takes care of that.
    H5.close dataset
