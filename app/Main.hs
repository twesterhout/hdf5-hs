module Main (main) where

import Data.HDF5
import Data.Proxy
import System.IO (IOMode (..))

main :: IO ()
main = do
  -- disableDiagOutput
  withFile "workspace_30.h5" ReadMode $ \handle ->
    withRoot handle $ \handle' ->
      byName handle' "/47" $ \(Some root@(Group _)) -> do
        -- print =<< getSize handle
        -- print =<< getSize root
        -- print =<< getDims root "c"
        () <- byName root "c" $ \(Some x@(Dataset _)) ->
          print =<< readDataset' (Proxy @Double) x
        print =<< readAttribute' (Proxy @Double) root "energy"

-- print =<< readAttribute' (Proxy @Double) root "energy"

-- print =<< h5Name handle
-- print =<< h5FindDataset handle "c"
-- g' <- h5OpenByIndex handle 0
-- print =<< h5Size g'
-- print =<< h5Name g'
-- print =<< h5FindDataset g' "tleft"
-- print =<< h5GetNumDims g' "tleft"
-- print =<< h5GetDatasetInfo g' "c"
-- readTypedDataset (Proxy @Double) g' "c" >>= \case
--   (H5ConcreteBlob v dims) -> print dims
-- readDataset g' "c" >>= \case
--   (H5DoubleBlob v dims) -> print dims
-- _ <- h5FoldlM (\i h -> h5Name h >>= \name -> print (i, name) >> return (i + 1)) 0 g'
-- closeHandle g'
