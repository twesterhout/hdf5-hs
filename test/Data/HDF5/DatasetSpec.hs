{-# LANGUAGE OverloadedLists #-}

module Data.HDF5.DatasetSpec (spec) where

import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import Data.HDF5
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import System.Directory
import Test.Hspec
import Test.Hspec.QuickCheck
import Utils

shouldRoundtrip :: (KnownDataset a, Allocatable IO a, Eq a, Show a) => a -> Expectation
shouldRoundtrip x =
  withFile "test_roundtrip.h5" WriteTruncate $ \h -> do
    exists h "Dataset" `shouldReturn'` False
    dset <- createDataset h "Dataset" x
    exists h "/Dataset" `shouldReturn'` True
    readDataset dset `shouldReturn'` x

spec :: Spec
spec = do
  it "creates datasets" $ do
    let f :: Text
        f = "new_empty_file.h5"
    doesFileExist (unpack f) `shouldReturn` False
    withFile f WriteTruncate $ \h -> do
      exists h "Dataset" `shouldReturn'` False
      dtype <- createDatatype @Float
      dspace <- createDataspace [3, 4]
      dset <- createUnfilledDataset h "Dataset" dtype dspace
      getDatatype dset `shouldReturn'` dtype
      exists h "Dataset" `shouldReturn'` True
    doesFileExist (unpack f) `shouldReturn` True
    removeFile (unpack f)
  prop "writes/reads Int" $ \(x :: Int) -> shouldRoundtrip (Scalar x)
  prop "writes/reads Float" $ \(x :: Float) -> shouldRoundtrip (Scalar x)
  prop "writes/reads Double" $ \(x :: Double) -> shouldRoundtrip (Scalar x)
  prop "writes/reads Text" $ \(s :: String) -> unless ('\0' `elem` s) $ shouldRoundtrip (pack s)
  prop "writes/reads ByteString" $ \(s :: String) -> unless ('\0' `elem` s) $ shouldRoundtrip (encodeUtf8 (pack s))
  prop "writes/reads [Int]" $ \(ListVector @Int x) -> shouldRoundtrip x
  prop "writes/reads [[Int]]" $ \(ListMatrix @Int x) -> shouldRoundtrip x
  prop "writes/reads [[[Int]]]" $ \(ListTensor3D @Int x) -> shouldRoundtrip x
  prop "writes/reads [Double]" $ \(ListVector @Double x) -> shouldRoundtrip x
  prop "writes/reads [[Double]]" $ \(ListMatrix @Double x) -> shouldRoundtrip x
  prop "writes/reads [[[Double]]]" $ \(ListTensor3D @Double x) -> shouldRoundtrip x

  -- removeFile "scalar_write.h5"
  -- it "writes slices to datasets" $ do
  --   let f :: Text
  --       f = "vector_slice.h5"
  --   withFile f WriteTruncate $ \h -> do
  --     exists h "Dataset" `shouldReturn'` False
  --     dtype <- createDatatype @Float
  --     dspace <- createDataspace [4]
  --     slice <- selectHyperslab (Hyperslab [1] [2] [2] [1]) dspace
  --     dset <- createUnfilledDataset h "Dataset" dtype dspace
  --     writeDatasetExplicit dset slice (V.fromList [1, 2] :: Vector Float)
  --   removeFile (unpack f)
  it "reads datasets inplace" $ do
    let f :: Text
        f = "vector_read.h5"
    withFile f WriteTruncate $ \h -> do
      exists h "Dataset" `shouldReturn'` False
      dset <- createDataset h "Dataset" (V.fromList [1, 2, 3, 4] :: Vector Float)
      exists h "Dataset" `shouldReturn'` True
      let buf = (V.replicate 4 0 :: Vector Float)
      readDatasetInto buf dset
      liftIO $ buf `shouldBe` V.fromList [1, 2, 3, 4]
    removeFile (unpack f)
