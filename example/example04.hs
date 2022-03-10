{-# LANGUAGE OverloadedStrings #-}

import qualified Data.HDF5 as H5
import Data.Some (withSome)
import Data.Text (pack)
import Data.Text.IO (putStrLn)
import qualified ListT
import Prelude hiding (putStrLn)

main :: IO ()
main =
  H5.withFile "groups.h5" H5.WriteTruncate $ \file -> do
    g <- H5.createGroup file "MyGroup"
    _ <- H5.createGroup file "MyGroup/Group_A"
    _ <- H5.createGroup g "Group_B"
    forM_ [(1 :: Int) .. 10] $ \i ->
      H5.createGroup g (pack $ show i)
    ListT.traverse_ pure $
      H5.forGroupM g $ \case
        x@H5.Group -> liftIO $ putStrLn $ H5.getName x
        _ -> liftIO $ putStrLn "not a group"
    pure ()
