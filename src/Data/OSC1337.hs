{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.OSC1337
  ( module Data.OSC1337,
    LatexStr (..),
    latex,
    math,
  )
where

import Codec.Picture
import Codec.Picture.Png (encodePng)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (encode)
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Lazy (toStrict)
import Data.Sixel.Internal
import System.IO.Unsafe (unsafePerformIO)

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

putImage :: FilePath -> IO ()
putImage file = do
  readImage file >>= \case
    Left err -> print err
    Right img -> putOSC img

instance Show LatexStr where
  show str = show $ toOSC str

instance ToOSC LatexStr where
  toOSC str = unsafePerformIO $ do
    latex2img str >>= \case
      Left err -> error err
      Right img -> return $ toOSC img
  putOSC img = putStr $ show $ toOSC img
