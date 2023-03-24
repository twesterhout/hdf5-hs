module Data.HDF5.FileSpec (spec) where

import Data.HDF5
import Data.Text
import System.Directory
import Test.Hspec

spec :: Spec
spec = do
  it "creates / opens files" $ do
    let f :: Text
        f = "new_empty_file.h5"
    doesFileExist (unpack f) `shouldReturn` False
    runHDF5 $
      withFile f WriteTruncate $
        const (pure ())
    doesFileExist (unpack f) `shouldReturn` True
    runHDF5 $
      withFile f ReadOnly $
        const (pure ())
    doesFileExist (unpack f) `shouldReturn` True
    runHDF5 $
      withFile f WriteAppend $
        const (pure ())
    doesFileExist (unpack f) `shouldReturn` True
    removeFile (unpack f)
