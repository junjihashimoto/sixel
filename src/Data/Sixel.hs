{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Data.Sixel where

import Codec.Picture
import Data.Char (chr)
import Data.Word (Word8)
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C.String
import           System.IO.Unsafe
import qualified Data.Vector.Storable          as V

foreign import ccall "bufsize" c_bufsize :: CInt -> CInt -> IO CInt
foreign import ccall "img2sixel" c_img2sixel :: Ptr () -> Ptr () -> CInt -> CInt -> IO CInt

img2sixel :: Image PixelRGB8 -> String
img2sixel img = unsafePerformIO $ do
  let (Image w h vec) = img
  bsize <- c_bufsize (fromIntegral w) (fromIntegral h)
  dptr <- mallocForeignPtrBytes (fromIntegral bsize)
  let (sptr,_) = V.unsafeToForeignPtr0 vec
  withForeignPtr dptr $ \dst -> do
    withForeignPtr sptr $ \src -> do
      len <- c_img2sixel (castPtr dst) (castPtr src) (fromIntegral w) (fromIntegral h)
      peekCStringLen (castPtr dst, fromIntegral len)

newtype SixelImage = SixelImage { toSixelString :: String } deriving (Eq)

instance Show SixelImage where
  show (SixelImage img) = img

type ColorNumber = Word8

type PixelPattern = Word8

type Width = Int

type Height = Int

data SixelCmd
  = Start Int Int Int
  | End
  | Size Int Int Int Int
  | ColorMapRGB ColorNumber Word8 Word8 Word8
  | ColorMapHLS ColorNumber Int Int Int
  | Color ColorNumber
  | Sixel PixelPattern
  | Repeat Int PixelPattern
  | CR
  | LF
  deriving (Eq)

instance Show SixelCmd where
  show = \case
    (Start p1 p2 p3) -> "\ESCP" ++ show p1 ++ ";" ++ show p2 ++ ";" ++ show p3 ++ "q"
    End -> "\ESC\\"
    (Size pan pad width height) -> concat ["\"", show pan, ";", show pad, ";", show width, ";", show height]
    (ColorMapRGB number x y z) -> concat ["#", show number, ";2;", show x, ";", show y, ";", show z]
    (ColorMapHLS number h l s) -> concat ["#", show number, ";1;", show h, ";", show l, ";", show s]
    (Color number) -> concat ["#", show number]
    (Sixel pat) -> [chr (fromIntegral pat + 0x3f)]
    (Repeat num pat) -> concat ["!", show num, [chr (fromIntegral pat + 0x3f)]]
    CR -> "$"
    LF -> "-"

instance {-# OVERLAPS #-} Show [SixelCmd] where
  show xs = concat $ map show xs

numDigits :: (Integral a, Ord a, Num a) => a -> Int
numDigits n
  | n < 10 = 1
  | otherwise = numDigits (n `div` 10) + 1

class ToSixel a where
  toSixel :: a -> SixelImage

instance ToSixel [SixelCmd] where
  toSixel xs = SixelImage (concat $ map show xs)

instance ToSixel DynamicImage where
  toSixel dimg = toSixel $ convertRGB8 dimg

instance ToSixel (Image PixelRGB8) where
  toSixel img = SixelImage (img2sixel img)
--  toSixel img = SixelImage (show (toSixelCmds img))

toSixelCmds :: Image PixelRGB8 -> [SixelCmd]
toSixelCmds img =
    let width = imageWidth img -1
        height = imageHeight img -1
        header =
          [ Start 8 1 0,
            Size 1 1 width height,
            ColorMapRGB 0 100 100 100,
            Color 0
          ]
        footer = End
        putSixel j = case j `mod` 6 of
          0 -> Sixel 1
          1 -> Sixel 2
          2 -> Sixel 4
          3 -> Sixel 8
          4 -> Sixel 16
          5 -> Sixel 32
        pixels =
          concat
            [ header,
              concat
                ( flip map [0 .. (height -1)] $ \j ->
                    concat
                      [ concat
                          ( flip map [0 .. (width -1)] $ \i ->
                              [ pixel2colorMap img i j,
                                putSixel j
                              ]
                          ),
                        if (j `mod` 6) == 5 then [LF] else [CR]
                      ]
                ),
              [footer]
            ]
     in pixels

pixel2colorMap :: Image PixelRGB8 -> Int -> Int -> SixelCmd
pixel2colorMap img i j =
  let p@(PixelRGB8 r g b) = pixelAt img i j
      rr = fromIntegral $ ((fromIntegral r :: Int) * 101) `div` 256
      gg = fromIntegral $ ((fromIntegral g :: Int) * 101) `div` 256
      bb = fromIntegral $ ((fromIntegral b :: Int) * 101) `div` 256
   in ColorMapRGB 0 rr gg bb

putImage :: FilePath -> IO ()
putImage file = do
  readImage file >>= \case
    Left err -> print err
    Right img -> putStr $ toSixelString $ toSixel img

demo :: [SixelCmd]
demo =
  [ Start 0 0 8,
    Size 1 1 100 100,
    ColorMapRGB 0 50 0 0,
    Color 0,
    Sixel 1,
    Sixel 2,
    Sixel 4,
    Sixel 8,
    Sixel 16,
    End
  ]
