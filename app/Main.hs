module Main (main) where

import Data.Complex
import Data.HDF5
import Data.Proxy
import Data.Text
import System.IO (IOMode (..))

main :: IO ()
main = do
  -- disableDiagOutput
  withFile "workspace_30.h5" ReadWriteMode $ \handle -> do
    byName handle "/47" $ \(Some root) -> do
      -- print =<< getSize handle
      -- print =<< getSize root
      -- print =<< getDims root "c"
      -- () <- byName root "c" $ \(Some x@(Dataset _)) ->
      --   print =<< readDataset' (Proxy @Double) x
      writeAttribute root "random" (12.3 :: Float)
      print =<< readAttribute @Double root "energy"
      print =<< readAttribute @Float root "random"
      writeAttribute root "random" (15.8 :: Float)
      print =<< readAttribute @Float root "random"
      writeAttribute root "cheers" (17.0 :+ (-1.8) :: Complex Float)
      print =<< readAttribute @(Complex Float) root "cheers"
      writeAttribute @Text root "ping pong" "This is awesome!"
      print =<< readAttribute @Text root "ping pong"

      print =<< exists root "c'"
      print =<< exists root "/48/tleft"

      () <- byName root "c" $ \case
        (Some x@(Dataset _)) -> do
          print =<< getDatasetDims x
          print =<< exists x "/47/tleft"
        _ -> error "expected c to be a dataset"

      () <- byName root "c" $ \case
        (Some x@(Dataset _)) -> do
          blob <- readDataset @Double x
          writeDataset root "p" blob
        _ -> error "expected c to be a dataset"

      return ()
    return ()

  putStrLn "Hello world!"

-- case hasStorable (Proxy @Float) of
--   Just _ -> print "cheers!"
--   Nothing -> print "fuck!"

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
