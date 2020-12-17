{-# LANGUAGE LambdaCase #-}

import qualified Graphics.Vega.VegaLite as V
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson (encodeFile)
import Data.String (fromString)
import Data.Sixel
import Data.OSC1337
import Codec.Picture
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import System.IO.Unsafe (unsafePerformIO)


plot = V.toVegaLite [ bkg, cars, V.mark V.Circle [], enc [] ]
cars =  V.dataFromUrl (fromString "https://vega.github.io/vega-datasets/data/cars.json") []

enc = V.encoding
        . V.position V.X [ V.PName (fromString "Horsepower"), V.PmType V.Quantitative ]
        . V.position V.Y [ V.PName (fromString "Miles_per_Gallon"), V.PmType V.Quantitative ]
        . V.color [ V.MName (fromString "Origin"), V.MmType V.Nominal ]

bkg = V.background (fromString "rgba(0, 0, 0, 0.05)")
  
vegalite2img :: V.VegaLite -> IO (Either String DynamicImage)
vegalite2img plot =
  withSystemTempDirectory "sixel" $ \dir' -> do
    let dir="/tmp"
    encodeFile (dir ++ "/plot.json") (V.fromVL plot)
    (_, outlog1, errlog1) <- readProcessWithExitCode "vl2svg" [dir ++ "/plot.json", dir ++ "/plot.svg"] ""
    (_, outlog2, errlog2) <- readProcessWithExitCode "rsvg-convert" [dir ++ "/plot.svg", "-o", dir ++ "/plot-rgba.png"] ""
    (_, outlog3, errlog3) <- readProcessWithExitCode "convert" [dir ++ "/plot-rgba.png", "-flatten", dir ++ "/plot.png"] ""
    readImage (dir ++ "/plot.png") >>= \case
      Left err -> return $ Left $ "can not read plot.png. \n" ++ err ++ "\n" ++ errlog1 ++ "\n" ++ outlog1
      Right img -> return $ Right img

instance ToOSC V.VegaLite where
  toOSC str = unsafePerformIO $ do
    vegalite2img str >>= \case
      Left err -> error err
      Right img -> return $ toOSC img
  putOSC img = putStr $ show $ toOSC img

main = do
  putStr.show.toOSC $ plot
