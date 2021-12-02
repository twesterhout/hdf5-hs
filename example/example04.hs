import qualified Data.HDF5 as H5

main :: IO ()
main =
  H5.withFile "groups.h5" H5.WriteTruncate $ \file -> do
    H5.createGroup file "/MyGroup"
    H5.createGroup file "/MyGroup/Group_A"
    g <- H5.open file "/MyGroup"
    H5.createGroup g "Group_B"
