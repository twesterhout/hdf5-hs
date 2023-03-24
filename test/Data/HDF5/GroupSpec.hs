module Data.HDF5.GroupSpec (spec) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import Data.HDF5
import Data.Text
import System.Directory
import Test.Hspec
import Utils

spec :: Spec
spec = do
  it "creates groups" $ do
    let f :: Text
        f = "new_empty_file.h5"
    doesFileExist (unpack f) `shouldReturn` False
    withFile f WriteTruncate $ \h -> do
      exists h "Group" `shouldReturn'` False
      exists h "/Group/Another" `shouldReturn'` False
      createGroup h "Group" >>= \g ->
        void $ createGroup g "Another"
      exists h "Group" `shouldReturn'` True
      exists h "Group/Another" `shouldReturn'` True
      exists h "/Group/Another" `shouldReturn'` True
    doesFileExist (unpack f) `shouldReturn` True
    removeFile (unpack f)
