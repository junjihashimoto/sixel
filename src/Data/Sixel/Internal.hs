{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Sixel.Internal where

import Codec.Picture
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

foreign import ccall "bufsize" c_bufsize :: CInt -> CInt -> IO CInt

foreign import ccall "img2sixel" c_img2sixel :: Ptr () -> Ptr () -> CInt -> CInt -> IO CInt
foreign import ccall "img2palettized_sixel" c_img2palettized_sixel :: Ptr () -> Ptr () -> Ptr () -> CInt -> CInt -> IO CInt

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

latex2img :: LatexStr -> IO (Either String DynamicImage)
latex2img (LatexStr str size) =
  withSystemTempDirectory "sixel" $ \dir -> do
    writeFile (dir ++ "/sixel.tex") (latexStr str size)
    (_, outlog, errlog) <- readProcessWithExitCode "pdflatex" ["-output-directory=" ++ dir, dir ++ "/sixel.tex"] ""
    readProcessWithExitCode "gs" ["-dSAFER", "-r75", "-sDEVICE=png16m", "-o", dir ++ "/sixel.png", dir ++ "/sixel.pdf"] ""
    readImage (dir ++ "/sixel.png") >>= \case
      Left err -> return $ Left $ "can not read sixel.png. \n" ++ err ++ "\n" ++ errlog ++ "\n" ++ outlog
      Right img -> return $ Right img
