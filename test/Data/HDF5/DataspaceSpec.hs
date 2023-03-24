{-# LANGUAGE OverloadedLists #-}

module Data.HDF5.DataspaceSpec (spec) where

import Control.Monad (forM_)
import Data.HDF5
import Test.Hspec
import Utils

spec :: Spec
spec = do
  it "creates scalar dataspaces" $ do
    runHDF5 $ do
      s <- createDataspace []
      getSelection s `shouldReturn'` Some SelectionAll
      isScalarDataspace s `shouldReturn'` True
  it "creates regular dataspaces" $ do
    runHDF5 $ do
      s <- createDataspace [2, 3]
      getSelection s `shouldReturn'` Some SelectionAll
      isScalarDataspace s `shouldReturn'` False
  it "selects hyperslabs" $ do
    runHDF5 $ do
      s <- createDataspace [4, 4]
      forM_
        ( [ Hyperslab [0, 0] [1, 1] [3, 3] [1, 1]
          , Hyperslab [0, 0] [1, 1] [1, 1] [1, 1]
          , Hyperslab [0, 0] [2, 2] [2, 2] [1, 1]
          , Hyperslab [1, 1] [1, 1] [1, 1] [2, 2]
          ]
            :: [Hyperslab]
        )
        $ \slab -> do
          s' <- selectHyperslab slab s
          getSelection s' `shouldReturn'` Some (SelectionHyperslabs (Just slab))
          isScalarDataspace s' `shouldReturn'` False

      s2 <-
        selectHyperslab (Hyperslab [1, 1] [1, 1] [1, 1] [2, 2]) s
          >>= selectHyperslabWith H5S_SELECT_AND (Hyperslab [0, 0] [1, 1] [1, 1] [2, 2])
      getSelection s2 `shouldReturn'` Some (SelectionHyperslabs (Just (Hyperslab [1, 1] [1, 1] [1, 1] [1, 1])))

      s3 <-
        selectHyperslab (Hyperslab [0, 0] [1, 1] [1, 1] [2, 2]) s
          >>= selectHyperslabWith H5S_SELECT_AND (Hyperslab [2, 2] [1, 1] [1, 1] [2, 2])
      getSelection s3 `shouldReturn'` Some SelectionNone
  it "constructs bounding boxes" $ do
    hyperslabBoundingBox (Hyperslab [0, 0] [1, 1] [1, 1] [2, 2]) `shouldBe` [2, 2]
    hyperslabBoundingBox (Hyperslab [0, 1] [2, 3] [3, 1] [2, 2]) `shouldBe` [8, 3]
