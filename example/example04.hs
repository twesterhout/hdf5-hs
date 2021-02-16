import qualified Data.HDF5.Types as H5
import qualified Data.HDF5.Wrapper as H5
import Data.Text (Text)
import qualified Data.Vector.Storable as V

main :: IO ()
main = do
  file <- H5.h5f_open "example/input_for_04.h5" H5.WriteAppend
  H5.h5l_iterate file $ \obj name ->
    print name
  group <- H5.h5o_open file "/A"
  attr <- H5.h5a_read @Text group "desc"
  print attr
  H5.h5a_write group "weight" (123.4 :: Double)
  H5.h5o_close group
  print "ping"

  group' <- H5.h5o_open file "B"
  dataset <- H5.h5d_open group' "d1"
  (v :: [Double]) <- H5.h5d_read dataset
  print v
  H5.h5o_close dataset

  dataset' <- H5.h5d_open group' "d2"
  ((_ :: [Int]), (_ :: [Int]), (v' :: V.Vector Int)) <- H5.h5d_read dataset'
  print v'
  H5.h5o_close dataset'
  H5.h5o_close group'
  H5.h5f_close file
