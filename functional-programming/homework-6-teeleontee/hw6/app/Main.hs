module Main (main) where

import HW6.T3

import Control.Monad
import Data.Parser

randomSeed :: Int
randomSeed = 2024

main :: IO ()
main = do
  (opts, conf) <- getOptionsAndConfig 
  let a = take (optIterations opts) (simulate randomSeed conf) 
  forM_ a $ printGrid (optGridSize opts)
