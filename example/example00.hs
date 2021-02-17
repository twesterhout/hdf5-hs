{-# LANGUAGE OverloadedStrings #-}

import qualified Data.HDF5 as H5

{- Reproduces "Create a dataset" example from HDF5 documentation:
   <https://raw.githubusercontent.com/HDFGroup/hdf5/develop/examples/h5_crtdat.c>
-}

main :: IO ()
main =
  -- Create a new file
  H5.withFile' "dset.h5" H5.WriteTruncate $ \file -> do
    -- Create a dataset
    join $
      H5.createDataset file "/dset"
        <$> (H5.ofShape [4, 6]) -- choose shape
        <*> (H5.ofType @Int) -- choose data type
