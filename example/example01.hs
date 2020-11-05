{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.HDF5 (IOMode (..), Some (..))
import qualified Data.HDF5 as H5
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  H5.withFile "test.h5" WriteMode $ \handle -> do
    -- Create a dataset
    H5.makeDataset handle "A" $ H5.Blob @Double [3, 3] $
      [1.0, 2.0, 3.0, -2.0, 4.0, 5.0, -3.0, -5.0, 6.0]
    -- Create a group
    H5.makeGroup handle "g"
  H5.withFile "test.h5" ReadMode $ \handle -> do
    -- Iterate over "/" and print names of all elements
    let f i (Some object) = do
          T.putStrLn =<< H5.getName object
          return (i + 1)
    count <- H5.foldM f (0 :: Int) handle
    T.putStrLn $ "'/' contains " <> T.pack (show count) <> " elements"
