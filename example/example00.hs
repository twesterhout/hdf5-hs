{-# LANGUAGE OverloadedStrings #-}

import qualified Data.HDF5 as H5
import Data.Text (Text)

main :: IO ()
main = do
  H5.withFile' "example00.h5" H5.WriteTruncate $ \file -> do
    -- Create a group
    H5.makeGroup file "myGroup"
    -- Open it
    H5.withGroup @Text file "myGroup" $ \group -> do
      -- Create a dataset
      H5.writeDataset @Text group "myDataset" [(1 :: Int) .. 25]
      -- Add a description
      H5.writeAttribute @Text group "myDataset" "myAttribute" ("Contains very important data 😀" :: Text)
