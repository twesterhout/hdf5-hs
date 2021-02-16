import qualified Data.HDF5.Wrapper as H5
import Data.Text (Text)

main :: IO ()
main = do
  file <- H5.h5f_open "example/input_for_04.h5" H5.ReadWrite
  H5.h5l_iterate file $ \obj name ->
    print name
  group <- H5.h5o_open file "/A"
  attr <- H5.h5a_read @Text group "desc"
  print attr
  H5.h5a_write group "weight" (123.4 :: Double)
  H5.h5o_close group
  H5.h5f_close file
