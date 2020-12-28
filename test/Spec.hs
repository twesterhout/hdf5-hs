module Main (main) where

import Data.HDF5
import System.Directory
import Test.Hspec

anyH5Exception :: Selector H5Exception
anyH5Exception = const True

main :: IO ()
main = hspec $ do
  describe "Files" $ do
    it "opens an existing file" $ do
      doesFileExist "test/reading_test.h5" `shouldReturn` True
      _ <- withFile "test/reading_test.h5" ReadMode (\(!_) -> return ())
      return ()
    it "throws an exceptions when file does not exist" $ do
      withFile "test/not_there.h5" ReadMode (\(!_) -> return ()) `shouldThrow` anyH5Exception
    it "truncates a file if it already exists" $ do
      copyFile "test/reading_test.h5" "test/truncate_test.h5"
      _ <- withFile "test/truncate_test.h5" WriteMode (\(!_) -> return ())
      isSmaller <- (<) <$> getFileSize "test/truncate_test.h5" <*> getFileSize "test/reading_test.h5"
      isSmaller `shouldBe` True
      removeFile "test/truncate_test.h5"
    it "appends to a file if it already exists" $ do
      copyFile "test/reading_test.h5" "test/append_test.h5"
      _ <- withFile "test/append_test.h5" ReadWriteMode (\(!_) -> return ())
      isSame <- (==) <$> getFileSize "test/append_test.h5" <*> getFileSize "test/reading_test.h5"
      isSame `shouldBe` True
      removeFile "test/append_test.h5"
    it "creates a file if it does not exist" $ do
      doesFileExist "test/a_non_existant_file.h5" `shouldReturn` False
      _ <- withFile "test/a_non_existant_file.h5" WriteMode (\(!_) -> return ())
      doesFileExist "test/a_non_existant_file.h5" `shouldReturn` True
      removeFile "test/a_non_existant_file.h5"
