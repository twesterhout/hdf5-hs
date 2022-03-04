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
    -- Scalar datasets can be created via the Scalar newtype
    H5.createDataset file "/dset3" $ H5.Scalar (1.0 :: Double)

    -- Read from a dataset
    (xs :: [[Int]]) <- H5.open file "/dset1" >>= H5.readDataset
    print xs

    -- Read part of a dataset
    -- Along the zeroth dimension we select 3 consequtive (i.e. stride is 1) elements
    -- starting at index 2
    H5.open file "/dset2"
      >>= (H5.sliceDataset 0 2 3 1 >>> H5.readSelected @(Vector Float))
      >>= print
    -- prints [3.0,4.0,5.0]

    -- Read scalar dataset
    H5.open file "/dset3" >>= H5.readDataset @(H5.Scalar Double) >>= (print . H5.unScalar)

    -- Update an existing dataset
    H5.open file "/dset2" >>= H5.writeDataset [(10 :: Float) .. 15]

    dataset <- H5.open file "/dset2"
    print =<< H5.readDataset @[Float] dataset
    -- Close the dataset early. Not strictly necessary, because ResourceT monad
    -- transformer takes care of that.
    H5.close dataset
