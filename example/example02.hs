{-# LANGUAGE OverloadedStrings #-}

import qualified Data.HDF5 as H5

main :: IO ()
main =
  H5.withFile "dset.h5" H5.WriteAppend $ \file -> do
    (dataset :: H5.Dataset) <- H5.open file "/dset1"
    H5.writeAttribute dataset "Units" [(100 :: Int), 200]
    H5.writeAttribute dataset "Comment" ("Hello world" :: Text)
    H5.readAttribute @[Int] dataset "Units" >>= print
    H5.readAttribute @Text dataset "Comment" >>= print
