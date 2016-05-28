module Interface where

import Logic
import Graphics.Gloss.Interface.Pure.Game

maxLen :: Task -> Int
maxLen = maximum . map length

drawNonogram :: Nonogram -> Picture
drawNonogram Nonogram { field = board
                      , cols  = top
                      , rows  = left
                      } = pictures[ drawFieldAt startPoint cellSize cellSize numCols numRows
                      --            , drawCols  startPoint top
                      --            , drawRows  startPoint left
                                  ]
  where
    numRows = length board
    numCols = length (head board)
    windowWidth = 500
    windowHeight = 500
    cellSize = 20
    lenLeft = cellSize * (maxLen left)
    lenTop = cellSize * (maxLen top)
    startPoint = ( (div ((-1) * windowWidth) 2) + lenLeft
                 , (div windowHeight 2) - lenTop
                 )

drawFieldAt :: Coord -> Int -> Int -> Int -> Int -> Picture
drawFieldAt (i, j) width height c r = pictures [ translate (fromIntegral i) (fromIntegral j) 
                                               $ scale (fromIntegral width) (fromIntegral height) 
                                               $ drawField c r
                                               ]

drawField :: Int -> Int -> Picture
drawField i j = pictures (map drawCell numCR)
  where
    numRow = [x | x <- [(negate j) .. (negate 1)]]
    numCol = [x | x <- [0 .. (i - 1)]]
    numCR = [(x, y) | x <- numCol, y <- numRow]

--drawCols :: Coord -> Task -> Picture
--drawCols _ = id

--drawRows :: Coord -> Task -> Picture
--drawRows _ = id

drawCell :: Coord -> Picture
drawCell (i, j) = pictures [ translate (fromIntegral i) (fromIntegral j) $ color white $ rectangleSolid size size
                           , translate (fromIntegral i) (fromIntegral j) $ color black $ rectangleWire size size
                           ]
--  | a == E = color white $ rectangleWire size size
--  | a == D = color black $ rectangleSolid size size
--  | a == U = pictures [ drawCell E
--                      , line[(0, 0), (size, size)]
--                      , line[(0, size), (size, 0)]
--                      ]
--  | otherwise = error "Invalid field by 'drawCell'"
  where
    size = 1


handleNonogram :: Event -> Nonogram -> Nonogram
handleNonogram _ = id

updateNonogram :: Float -> Nonogram -> Nonogram
updateNonogram _ = id
