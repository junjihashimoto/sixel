{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.OSC1337 where

import Codec.Picture
import Codec.Picture.Png (encodePng)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (encode)
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Lazy (toStrict)
import Data.Sixel (LatexStr (..), latex, math)
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)

data OSCCmd
  = Start
  | End
  | FileName String
  | Size Int
  | Width Int
  | Height Int
  | PreserveAspectRatio Int
  | Inline Int
  | Align String
  | MimeType String
  | ImageDat String
  deriving (Eq)

instance Show OSCCmd where
  show = \case
    Start -> "\ESC]1337;"
    End -> "\a"
    Size s -> "size=" ++ show s ++ ";"
    Width s -> "width=" ++ show s ++ ";"
    Height s -> "height=" ++ show s ++ ";"
    PreserveAspectRatio s -> "preserveAspectRatio=" ++ show s ++ ";"
    Inline s -> "inline=" ++ show s ++ ";"
    Align s -> "align=" ++ s ++ ";"
    MimeType s -> "type=" ++ s ++ ";"
    ImageDat s -> BC.unpack $ encode $ BC.pack s

instance {-# OVERLAPS #-} Show [OSCCmd] where
  show xs = concat $ map show xs

newtype OSCImage = OSCImage {toOSCString :: String} deriving (Eq)

instance Show OSCImage where
  show (OSCImage img) = img

-- | See https://chromium.googlesource.com/apps/libapps/+/master/hterm/doc/ControlSequences.md#OSC-1337
class ToOSC a where
  toOSC :: a -> OSCImage
  putOSC :: a -> IO ()

instance {-# OVERLAPS #-} (Show a) => ToOSC a where
  toOSC xs = OSCImage $ show xs
  putOSC xs = putStrLn $ show xs

img2osc :: Image PixelRGB8 -> ByteString
img2osc img =
  let (Image w h _) = img
      dat = toStrict $ encodePng img
   in B.concat
        [ "\ESC]1337;File=name=",
          encode "display.png",
          ";",
          "width=" <> BC.pack (show w) <> "px;",
          "height=" <> BC.pack (show h) <> "px;",
          "inline=1;:",
          encode dat,
          "\a"
        ]

instance {-# OVERLAPS #-} ToOSC DynamicImage where
  toOSC dimg = toOSC $ convertRGB8 dimg
  putOSC img = putOSC $ convertRGB8 img

instance {-# OVERLAPS #-} ToOSC (Image PixelRGB8) where
  toOSC img = OSCImage (BC.unpack $ img2osc img)
  putOSC img = B.putStr $ img2osc img

putOSCImage :: FilePath -> IO ()
putOSCImage file = do
  readImage file >>= \case
    Left err -> print err
    Right img -> putOSC img

data LatexStr
  = LatexStr
      { toLatexStr :: String,
        strSize :: Float
      }
  deriving (Eq)

latex :: String -> LatexStr
latex str = LatexStr str 2.5

math :: String -> LatexStr
math str = LatexStr ("$" ++ str ++ "$") 2.5

instance Show LatexStr where
  show str = show $ toOSC str

latexStr :: String -> Float -> String
latexStr str size =
  "\\documentclass[border=2pt]{standalone}"
    ++ "\\usepackage{amsmath}"
    ++ "\\usepackage{graphicx}"
    ++ "\\usepackage{varwidth}"
    ++ "\\begin{document}"
    ++ "\\begin{varwidth}{\\linewidth}"
    ++ "\\scalebox{"
    ++ show size
    ++ "}{"
    ++ str
    ++ "}"
    ++ "\\end{varwidth}"
    ++ "\\end{document}"

instance ToOSC LatexStr where
  toOSC (LatexStr str size) = unsafePerformIO $ do
    withSystemTempDirectory "osc1337" $ \dir -> do
      writeFile (dir ++ "/osc1337.tex") (latexStr str size)
      (_, outlog, errlog) <- readProcessWithExitCode "pdflatex" ["-output-directory=" ++ dir, dir ++ "/osc1337.tex"] ""
      readProcessWithExitCode "convert" [dir ++ "/osc1337.pdf", "-quality", "90", dir ++ "/osc1337.png"] ""
      readImage (dir ++ "/osc1337.png") >>= \case
        Left err -> error $ "can not read osc1337.png. // " ++ errlog ++ " // " ++ outlog
        Right img -> return $ toOSC img
  putOSC img = putStr $ show $ toOSC img
