{-# LANGUAGE OverloadedStrings #-}

import qualified Data.HDF5 as H5

main :: IO ()
main =
  H5.withFile "group.h5" H5.WriteTruncate $ \file -> do
    H5.createGroup file "/MyGroup"
