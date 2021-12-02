{-# LANGUAGE OverloadedStrings #-}

import qualified Data.HDF5 as H5

main :: IO ()
main =
  H5.withFile "dset.h5" H5.WriteAppend $ \file -> do
    (dataset :: H5.Dataset) <- H5.open file "/dset"
    H5.writeAttribute dataset "Units" [(100 :: Int), 200]
