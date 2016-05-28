module Interface where

import Logic
import Graphics.Gloss.Interface.Pure.Game

maxLen :: Task -> Int
maxLen = maximum . map length

drawNonogram :: Nonogram -> Picture
drawNonogram Nonogram { cols  = top
                      , rows  = left
                      } = pictures [ drawGrid startPoint cellSize cellSize (makeTopGrid top)
                                   , drawGrid startPoint cellSize cellSize (makeBotGrid left)
                                   , drawGrid startPoint cellSize cellSize emptyGrid
                                   ]
  where
    windowWidth = 500
    windowHeight = 500
    cellSize = 20
    lenLeft = cellSize * (maxLen left) + (div cellSize 2)
    lenTop = cellSize * (maxLen top) - (div cellSize 2)
    startPoint = ( (div ((-1) * windowWidth) 2) + lenLeft
                 , (div windowHeight 2) - lenTop
                 )
    emptyGrid = zip (repeat 0) [(x, y) | x <- [0 .. ((length top) - 1)]
                                       , y <- [(negate (length left)) .. -1]
                                       ]

makeBotGrid :: Task -> [(Int, Coord)]
makeBotGrid side = zip (foldr (++) [] side) (makeBotAccordance numXY)
  where
    numX  = [[(negate x) .. (negate 1)] | x <- map length side]
    numXY = zip numX (reverse [(negate (length numX)) .. (negate 1)])

makeBotAccordance :: [([Int], Int)] -> [Coord]
makeBotAccordance = foldr (++) [] . map (\x -> zip (fst x) (repeat (snd x)))

makeTopGrid :: Task -> [(Int, Coord)]
makeTopGrid side = zip (foldr (++) [] side) (makeTopAccordance numXY)
  where
    numY  = [[0 .. (x - 1)] | x <- map length side]
    numXY = zip [0 .. ((length numY) - 1)] numY

makeTopAccordance :: [(Int, [Int])] -> [Coord]
makeTopAccordance = foldr (++) [] . map (\x -> zip (repeat (fst x)) (reverse (snd x)))

drawGrid :: Coord -> Int -> Int -> [(Int, Coord)] -> Picture
drawGrid (i, j) width height = translate (fromIntegral i) (fromIntegral j) 
                             . scale (fromIntegral width) (fromIntegral height) 
                             . pictures 
                             . map drawCell


drawCell :: (Int, Coord) -> Picture
drawCell (n, (i, j))
  | (n == 0)  = pictures [ translate x y 
                         $ color white 
                         $ rectangleSolid size size
                         , translate x y 
                         $ color black 
                         $ rectangleWire size size
                         ]
  | otherwise = pictures [ translate x y 
                         $ color white 
                         $ rectangleSolid size size
                         , translate x y
                         $ color black 
                         $ rectangleWire size size
                         , translate (x - 0.25) (y - 0.25) 
                         $ scale 0.005 0.005
                         $ color black 
                         $ text (show x)
                         ]
  where 
    size = 1;
       x = fromIntegral i;
       y = fromIntegral j


handleNonogram :: Event -> Nonogram -> Nonogram
handleNonogram _ = id

updateNonogram :: Float -> Nonogram -> Nonogram
updateNonogram _ = id
