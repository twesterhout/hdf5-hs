{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.HDF5 (IOMode (..), Some (..))
import qualified Data.HDF5 as H5

createGroupIfNotPresent parent name = do
  alreadyCreated <- H5.exists parent name
  unless alreadyCreated $ H5.makeGroup parent name

prepare =
  H5.withFile "test.h5" ReadWriteMode $ \file -> do
    createGroupIfNotPresent file "g"
    H5.byName file "g" . H5.matchM @H5.Group $ \group ->
      createGroupIfNotPresent group "A"

main :: IO ()
main = do
  prepare
  H5.withFile "test.h5" ReadMode $ \file ->
    H5.byName file "g/A" $ \(Some object) ->
      print =<< H5.getName object
