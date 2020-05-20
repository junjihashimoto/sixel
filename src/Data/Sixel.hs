{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module Data.Sixel where

import Codec.Picture
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Internal as B
import Data.Char (chr)
import qualified Data.Vector.Storable as V
import Data.Word (Word8)
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)

foreign import ccall "bufsize" c_bufsize :: CInt -> CInt -> IO CInt

foreign import ccall "img2sixel" c_img2sixel :: Ptr () -> Ptr () -> CInt -> CInt -> IO CInt

newtype SixelImage = SixelImage {toSixelString :: String} deriving (Eq)

data LatexStr
  = LatexStr
      { toLatexStr :: String,
        strSize :: Float
      }
  deriving (Eq)

latex :: String -> LatexStr
latex str = LatexStr str 2.5

math :: String -> LatexStr
math str = LatexStr ("$"++str++"$") 2.5

instance Show SixelImage where
  show (SixelImage img) = img

instance Show LatexStr where
  show str = show $ toSixel str

type ColorNumber = Word8

type PixelPattern = Word8

type Width = Int

type Height = Int

data SixelCmd
  = Start Int Int Int
  | End
  | Size Int Int Width Height
  | ColorMapRGB ColorNumber Word8 Word8 Word8
  | ColorMapHLS ColorNumber Int Word8 Word8
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

class ToSixel a where
  toSixel :: a -> SixelImage
  putSixel :: a -> IO ()

instance {-# OVERLAPS #-} (Show a) => ToSixel a where
  toSixel xs = SixelImage $ show xs
  putSixel xs = putStrLn $ show xs

instance {-# OVERLAPS #-} ToSixel [SixelCmd] where
  toSixel xs = SixelImage (concat $ map show xs)
  putSixel xs = putStr $ concat $ map show xs

instance {-# OVERLAPS #-} ToSixel DynamicImage where
  toSixel dimg = toSixel $ convertRGB8 dimg
  putSixel img = BC.putStr $ img2sixel $ convertRGB8 img

instance {-# OVERLAPS #-} ToSixel (Image PixelRGB8) where
  toSixel img = SixelImage (BC.unpack $ img2sixel img)
  putSixel img = BC.putStr $ img2sixel img

instance {-# OVERLAPS #-} ToSixel SixelImage where
  toSixel = id
  putSixel img = putStr $ show img

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

instance ToSixel LatexStr where
  toSixel (LatexStr str size) = unsafePerformIO $ do
    withSystemTempDirectory "sixel" $ \dir -> do
      writeFile (dir ++ "/sixel.tex") (latexStr str size)
      (_,outlog,errlog) <- readProcessWithExitCode "pdflatex" ["-output-directory="++dir ,dir ++ "/sixel.tex"] ""
      readProcessWithExitCode "convert" [dir ++ "/sixel.pdf", "-quality", "90", dir ++ "/sixel.png"] ""
      readImage (dir ++ "/sixel.png") >>= \case
        Left err -> error $ "can not read sixel.png. // " ++ errlog ++ " // " ++ outlog
        Right img -> return $ toSixel img
  putSixel img = putStr $ show $ toSixel img

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
  where
    pixel2colorMap :: Image PixelRGB8 -> Int -> Int -> SixelCmd
    pixel2colorMap img i j =
      let p@(PixelRGB8 r g b) = pixelAt img i j
          rr = fromIntegral $ ((fromIntegral r :: Int) * 101) `div` 256
          gg = fromIntegral $ ((fromIntegral g :: Int) * 101) `div` 256
          bb = fromIntegral $ ((fromIntegral b :: Int) * 101) `div` 256
       in ColorMapRGB 0 rr gg bb

img2sixel :: Image PixelRGB8 -> ByteString
img2sixel img = unsafePerformIO $ do
  let (Image w h vec) = img
  bsize <- c_bufsize (fromIntegral w) (fromIntegral h)
  let (sptr, _) = V.unsafeToForeignPtr0 vec
  B.createAndTrim (fromIntegral bsize) $ \dst -> do
    withForeignPtr sptr $ \src -> do
      len <- c_img2sixel (castPtr dst) (castPtr src) (fromIntegral w) (fromIntegral h)
      return (fromIntegral len)

-- | Display sixel image via ByteString
-- putStr of String is really slow on ghci. (Compiled version is not so slow.)
-- To improve perfomance of rendering on ghci, this function uses putStr of ByteString.
putImage :: FilePath -> IO ()
putImage file = do
  readImage file >>= \case
    Left err -> print err
    Right img -> putSixel img
