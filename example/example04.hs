import qualified Data.HDF5 as H5

main :: IO ()
main =
  H5.withFile "groups.h5" H5.WriteTruncate $ \file -> do
    g <- H5.createGroup file "/MyGroup"
    _ <- H5.createGroup file "/MyGroup/Group_A"
    _ <- H5.createGroup g "Group_B"
    pure ()
