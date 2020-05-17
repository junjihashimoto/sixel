module Main where

import Data.Sixel
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let file = args !! 0
  putImage file
