module Main where

import Logic
import Interface
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = do
  path <- getLine
  nonogram <- importNonogram path
  play display bgColor fps nonogram drawNonogram handleNonogram updateNonogram
  where
   display = InWindow "Nongram" (500, 500) (100, 100)
   bgColor = (greyN 0.25)
   fps     = 30
