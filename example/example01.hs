{-# LANGUAGE OverloadedStrings #-}

import Control.Exception.Safe (assert)
import Control.Monad.Trans.Resource
import qualified Data.HDF5 as H5
import Data.Vector.Storable (MVector, Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

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
    -- dataspace <- H5.getDataspace dataset
    -- print $ H5.dataspaceSelectionType dataspace
    -- let h = H5.getHyperslab dataspace
    -- print $ h
    -- dataspace' <-
    --   H5.selectHyperslab (H5.sliceHyperslab 1 2 (-1) 2 $ H5.sliceHyperslab 0 0 3 1 $ h) dataspace
    -- print $ H5.getHyperslab dataspace'
    let (H5.DatasetSlice _ hyperslab) =
          H5.slice {- dim -} 1 {- start -} 2 {- count -} (-1 {- stride -}) 2 $
            H5.slice {- dim -} 0 {- start -} 0 {- count -} 3 {- stride -} 1 $ dataset
    -- print $ hyperslab

    dataspace <- H5.getDataspace dataset
    buf <- MV.new (4 * 6)
    liftIO $
      MV.unsafeWith buf $ \bufPtr -> runResourceT $ do
        let view :: H5.ArrayView' Int
            view = H5.ArrayView' bufPtr [4, 6] [6, 1]
            selection = H5.DatasetSlice dataset (H5.getHyperslab dataspace)
        H5.readSelectedInplace view selection
    print =<< V.freeze buf

    -- Close the dataset early. Not strictly necessary, because ResourceT monad
    -- transformer takes care of that.
    H5.close dataset

    -- Check data
    assert (v == V.fromList [1 .. 24]) $ return ()
