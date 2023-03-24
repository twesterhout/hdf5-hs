module Utils
  ( shouldReturn'
  , io
  , ListVector (..)
  , ListMatrix (..)
  , ListTensor3D (..)
  , ListTensor4D (..)
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.HDF5
import Test.Hspec
import Test.QuickCheck

infix 1 `shouldReturn'`

shouldReturn' :: (Eq a, Show a) => HDF5 s IO a -> a -> HDF5 s IO ()
shouldReturn' action expected = do
  value <- action
  liftIO $ value `shouldBe` expected

io :: IO a -> IO a
io = id

newtype ListVector a = ListVector [a]
  deriving stock (Show, Eq)

newtype ListMatrix a = ListMatrix [[a]]
  deriving stock (Show, Eq)

newtype ListTensor3D a = ListTensor3D [[[a]]]
  deriving stock (Show, Eq)

newtype ListTensor4D a = ListTensor4D [[[[a]]]]
  deriving stock (Show, Eq)

instance Arbitrary a => Arbitrary (ListVector a) where
  arbitrary = ListVector <$> listOf arbitrary

instance Arbitrary a => Arbitrary (ListMatrix a) where
  arbitrary = do
    d0 <- chooseInt (0, 50)
    d1 <- chooseInt (0, 50)
    ListMatrix <$> vectorOf d0 (vector d1)

instance Arbitrary a => Arbitrary (ListTensor3D a) where
  arbitrary = do
    d0 <- chooseInt (0, 20)
    d1 <- chooseInt (0, 20)
    d2 <- chooseInt (0, 20)
    ListTensor3D <$> vectorOf d0 (vectorOf d1 (vector d2))

instance Arbitrary a => Arbitrary (ListTensor4D a) where
  arbitrary = do
    d0 <- chooseInt (0, 10)
    d1 <- chooseInt (0, 10)
    d2 <- chooseInt (0, 10)
    d3 <- chooseInt (0, 10)
    ListTensor4D <$> vectorOf d0 (vectorOf d1 (vectorOf d2 (vector d3)))
