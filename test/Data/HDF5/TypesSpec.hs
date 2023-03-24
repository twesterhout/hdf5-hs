module Data.HDF5.TypesSpec (spec) where

import Data.HDF5
import Test.Hspec

spec :: Spec
spec = do
  it "return 123" $ do
    pure (123 :: Int) `shouldReturn` 123
