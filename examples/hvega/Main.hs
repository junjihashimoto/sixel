{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Graphics.Vega.VegaLite
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


plot = toVegaLite [ bkg, cars, mark Circle [], enc [] ]
cars =  dataFromUrl (fromString "https://vega.github.io/vega-datasets/data/cars.json") []

enc = encoding
        . position X [ PName (fromString "Horsepower"), PmType Quantitative ]
        . position Y [ PName (fromString "Miles_per_Gallon"), PmType Quantitative ]
        . color [ MName (fromString "Origin"), MmType Nominal ]

bkg = background (fromString "rgba(0, 0, 0, 0.05)")
  
vegalite2img :: VegaLite -> IO (Either String DynamicImage)
vegalite2img plot =
  withSystemTempDirectory "sixel" $ \dir' -> do
    let dir="/tmp"
    encodeFile (dir ++ "/plot.json") (fromVL plot)
    (_, outlog1, errlog1) <- readProcessWithExitCode "vl2svg" [dir ++ "/plot.json", dir ++ "/plot.svg"] ""
    (_, outlog2, errlog2) <- readProcessWithExitCode "rsvg-convert" [dir ++ "/plot.svg", "-o", dir ++ "/plot-rgba.png"] ""
    (_, outlog3, errlog3) <- readProcessWithExitCode "convert" [dir ++ "/plot-rgba.png", "-flatten", dir ++ "/plot.png"] ""
    readImage (dir ++ "/plot.png") >>= \case
      Left err -> return $ Left $ "can not read plot.png. \n" ++ err ++ "\n" ++ errlog1 ++ "\n" ++ outlog1
      Right img -> return $ Right img

instance ToOSC VegaLite where
  toOSC str = unsafePerformIO $ do
    vegalite2img str >>= \case
      Left err -> error err
      Right img -> return $ toOSC img
  putOSC img = putStr $ show $ toOSC img

res :: Int
res = 50

step :: Double
step=2*3.141592/fromIntegral res

xydat :: (Double -> Double) -> [(Double,Double)]
xydat func = map (\i-> (i,func i)) $ take res [0,step..] 

axis func = PAxis [ AxValues (Numbers (map fst (xydat func)))]

enc2 func = encoding
             . position X [ PName "X", PmType Quantitative, axis func]
             . position Y [ PName "Y", PmType Quantitative ]
             . color [ MName "Lines", MmType Nominal ]

dat' func = foldl
              (\sum' (x,y) ->
                 sum' .
                 dataRow [ ("X", Number x)
                         , ("Y", Number y)
                         , ("Lines",   Str "func")
                         ]
              )
              (dataFromRows [])
              (xydat func)
plot2 func = toVegaLite
        [ (dat' func) []
        , mark Line []
        , (enc2 func) []
        , height 300
        , width 400
        ]
 
main = do
  putStr.show.toOSC $ plot
  putStr.show.toOSC $ plot2 sin
