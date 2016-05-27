module Main where

import Logic
import Interface
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = do
  nonogram <- importNonogram "examples/lambda.txt"
  play display bgColor fps nonogram drawNonogram handleNonogram updateNonogram
  where
   display = InWindow "Nongram" (500, 500) (100, 100)
   bgColor = white
   fps     = 30
