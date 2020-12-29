{-# LANGUAGE OverloadedStrings #-}

import Data.HDF5 (Dataset, Group, IOMode (..))
import qualified Data.HDF5 as H5
import Data.Text (Text)

main :: IO ()
main = do
  H5.withFile "example00.h5" WriteMode $ \file -> do
    -- Create a group
    H5.makeGroup file "myGroup"
    -- Open it
    H5.byName file "myGroup" . H5.matchM @Group $ \group -> do
      -- Create a dataset
      H5.writeDataset group "myDataset" [(1 :: Int) .. 25]
      -- Add a description
      H5.byName group "myDataset" . H5.matchM @Dataset $ \dataset ->
        H5.writeAttribute dataset "myAttribute" ("Contains very important data 😀" :: Text)
