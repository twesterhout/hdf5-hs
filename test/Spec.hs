module Main (main) where

import Data.Complex
import Data.HDF5
import qualified Data.Vector.Storable as V
import System.Directory
import Test.Hspec
import Prelude hiding (withFile)

anyH5Exception :: Selector H5Exception
anyH5Exception = const True

main :: IO ()
main = hspec $ do
  describe "Files" $ do
    it "opens an existing file" $ do
      doesFileExist "test/reading_test.h5" `shouldReturn` True
      _ <- withFile "test/reading_test.h5" ReadOnly (\_ -> return ())
      return ()
    it "throws an exceptions when file does not exist" $ do
      withFile "test/not_there.h5" ReadOnly (\_ -> return ()) `shouldThrow` anyH5Exception
    it "truncates a file if it already exists" $ do
      copyFile "test/reading_test.h5" "test/truncate_test.h5"
      _ <- withFile "test/truncate_test.h5" WriteTruncate (\_ -> return ())
      isSmaller <- (<) <$> getFileSize "test/truncate_test.h5" <*> getFileSize "test/reading_test.h5"
      isSmaller `shouldBe` True
      removeFile "test/truncate_test.h5"
    it "appends to a file if it already exists" $ do
      copyFile "test/reading_test.h5" "test/append_test.h5"
      _ <- withFile "test/append_test.h5" WriteAppend (\_ -> return ())
      isSame <- (==) <$> getFileSize "test/append_test.h5" <*> getFileSize "test/reading_test.h5"
      isSame `shouldBe` True
      removeFile "test/append_test.h5"
    it "creates a file if it does not exist" $ do
      doesFileExist "test/a_non_existant_file.h5" `shouldReturn` False
      _ <- withFile "test/a_non_existant_file.h5" WriteTruncate (\_ -> return ())
      doesFileExist "test/a_non_existant_file.h5" `shouldReturn` True
      removeFile "test/a_non_existant_file.h5"
  describe "Datasets" $ do
    it "writes strided datasets" $ do
      (matrix :: TemporaryStridedMatrix Float) <-
        withFile' "test/strided_test_file.h5" WriteTruncate $ \g -> do
          writeDataset g "A" $ TemporaryStridedMatrix (2, 3) 4 (V.fromList [(1 :: Float) .. 8])
          openDataset @Text g "A" >>= readDataset
      matrix `shouldBe` (TemporaryStridedMatrix (2, 3) 3 (V.fromList [1, 2, 3, 5, 6, 7]))
      removeFile "test/strided_test_file.h5"
    it "writes complex datasets" $ do
      xs <-
        withFile' "test/complex_test_file.h5" WriteTruncate $ \g -> do
          writeDataset g "A" $ [((1 :: Double) :+ 2), ((-5) :+ 0.1)]
          openDataset @Text g "A" >>= readDataset
      xs `shouldBe` [((1 :: Double) :+ 2), ((-5) :+ 0.1)]
      removeFile "test/complex_test_file.h5"

-- removeFile "test/strided_test_file.h5"
