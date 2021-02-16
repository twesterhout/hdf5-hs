import qualified Data.HDF5.Wrapper as H5

main :: IO ()
main = do
  file <- H5.h5f_open "example/input_for_04.h5" H5.ReadOnly
  H5.h5l_iterate file $ \obj name ->
    print name
  H5.h5f_close file
