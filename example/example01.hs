{-# LANGUAGE OverloadedStrings #-}

import Control.Exception.Safe (assert)
import qualified Data.HDF5 as H5
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V

{- Reproduces "Read and write to a dataset" example from HDF5 documentation:
   <https://raw.githubusercontent.com/HDFGroup/hdf5/develop/examples/h5_rdwt.c>
-}

main :: IO ()
main =
  -- Open an existing file
  H5.withFile' "dset.h5" H5.WriteAppend $ \file -> do
    -- Write to dataset
    H5.writeDataset file "/dset" $
      H5.TemporaryContiguousArray [4, 6] (V.fromList @Int [1 .. 24])

    -- Read from a dataset
    dataset <- H5.openDataset @Text file "/dset"
    (H5.TemporaryContiguousArray _ (v :: Vector Int)) <- H5.readDataset dataset

    -- Close the dataset early. Not strictly necessary, because ResourceT monad
    -- transformer takes care of that.
    H5.close dataset

    -- Check data
    assert (v == V.fromList [1 .. 24]) $ return ()
