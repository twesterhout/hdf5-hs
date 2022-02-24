{-# LANGUAGE OverloadedStrings #-}

import qualified Data.HDF5 as H5

{- Reproduces "Create a dataset" example from HDF5 documentation:
   <https://raw.githubusercontent.com/HDFGroup/hdf5/develop/examples/h5_crtdat.c>
-}

main :: IO ()
main =
  -- Create a new file
  H5.withFile "dset.h5" H5.WriteTruncate $ \file -> do
    -- Create a dataset
    _ <-
      join $
        H5.createEmptyDataset file "/dset"
          <$> (H5.ofType @Int) -- choose data type
          <*> (H5.ofShape [4, 6]) -- choose shape

    -- Alternatively, if you're not a fan of combinators
    dspace <- H5.ofShape [4, 6]
    dtype <- H5.ofType @Int
    _ <- H5.createEmptyDataset file "/dset2" dtype dspace
    return ()
