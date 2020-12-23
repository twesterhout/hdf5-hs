{-# LANGUAGE OverloadedStrings #-}

import Data.HDF5 (IOMode (..))
import qualified Data.HDF5 as H5

main :: IO ()
main = do
  H5.withFile "test.h5" WriteMode $ \file -> do
    H5.makeGroup file "g"
    H5.byName file "g" . H5.matchM @H5.Group $ \g ->
      H5.writeDataset g "dataset1" [1, 2, 3, 4 :: Int]
  H5.withFile "test.h5" ReadMode $ \file -> do
    H5.byName file "g/dataset1" . H5.matchM @H5.Dataset $ \d ->
      H5.readDataset @Int d >>= print
