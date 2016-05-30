module Interface where

import Logic
import Solver
import Data.List
import Graphics.Gloss.Interface.Pure.Game

maxLen :: Task -> Int
maxLen = maximum . map length

-- | Генератор рисунка из nonogram.
drawNonogram :: Nonogram -> Picture
drawNonogram nonogram@Nonogram { solve = board
                      , cols  = top
                      , rows  = left
                      } 
  -- Рисуем подсказки и поле, совмещая 3 картинки.
  = pictures [ drawGrid startPoint cellSize cellSize (makeTopGrid top) 
             , drawGrid startPoint cellSize cellSize (makeBotGrid left)
             , drawBoardGrid startPoint cellSize cellSize (makeGrid board)
             , drawThickLines nonogram startPoint cellSize cellSize
             ]
  where
    lenLeft = cellSize * (maxLen left) + (div cellSize 2)
    lenTop = cellSize * (maxLen top) - (div cellSize 2)
    startPoint = ( (div ((-1) * windowWidth) 2) + lenLeft
                 , (div windowHeight 2) - lenTop
                 )

drawThickLines :: Nonogram -> Coord -> Int -> Int -> Picture
drawThickLines Nonogram {solve = board
                      , cols  = top
                      , rows  = left} 
                      (i, j) width height 
  = translate (fromIntegral i) (fromIntegral j) 
    (scale (fromIntegral width) (fromIntegral height) 
    (translate (-0.5) (-0.5)
    (pictures (vertical ++ horizontal))))
  where
    vertical = take (colBlockNum board) (drawVertLines (colBounds top board))
    horizontal = take (rowBlockNum board) (drawHorLines (rowBounds left board))
                                                

--colBounds :: Task -> Field -> (Int, Int)
--colBounds top  = rowBounds top . transpose

--rowBounds :: Task -> Field -> (Int, Int)
--rowBounds left board = (maxLen left, length board)

rowBounds :: Task -> Field -> (Int, Int)
rowBounds top  = colBounds top . transpose

colBounds :: Task -> Field -> (Int, Int)
colBounds left board = (maxLen left, length board)


colBlockNum :: Field -> Int
colBlockNum  = rowBlockNum . transpose 

rowBlockNum :: Field -> Int
rowBlockNum board = (length board - 1) `div` 5 + 1 

drawVertLine :: (Int, Int) -> Picture
drawVertLine (y1, y2) = translate 0 (fromIntegral y1 - height/2) (rectangleSolid width height)
  where 
      width = 0.2
      height = fromIntegral (y1 + y2)

drawHorLine :: (Int, Int) -> Picture
drawHorLine (x1, x2) = translate (width/2 - fromIntegral x1) 0 (rectangleSolid width height)
  where 
      width = fromIntegral (x2 + x1)
      height = 0.2

drawVertLines :: (Int, Int) -> [Picture]
drawVertLines bounds = [ translate x 0 (drawVertLine bounds) | x <- [0, 5 ..]]

drawHorLines :: (Int, Int) -> [Picture]
drawHorLines bounds = [ translate 0 y (drawHorLine bounds) | y <- [0, -5 ..]]




-- | Вычисляем координаты ячеек в IV четверти для дальнейшего translate во точку startPoint.
makeGrid :: Field -> [(Cell, Coord)]
makeGrid board = zip (foldr (++) [] (transpose board)) [
                                                      (x, y) | 
                                                      x <- [0 .. length (head board) - 1]
                                                      ,
                                                      y <- reverse [negate (length board) .. -1]
                                                      ]

-- | Нарисовать поле.
drawBoardGrid :: Coord -> Int -> Int -> [(Cell, Coord)] -> Picture
drawBoardGrid (i, j) width height = translate (fromIntegral i) (fromIntegral j) 
                                  . scale (fromIntegral width) (fromIntegral height) 
                                  . pictures 
                                  . map drawBoardCell

-- | Нарисовать ячейки поля.
drawBoardCell :: (Cell, Coord) -> Picture
drawBoardCell (n, (i, j))
  | (n == E)  = pictures [ translate x y 
                         $ color white 
                         $ rectangleSolid size size
                         , translate x y 
                         $ color black 
                         $ rectangleWire size size
                         ]
  | (n == D)  = translate x y 
              $ color black
              $ rectangleSolid size size
  | (n == U)  = pictures [ translate x y 
                         $ color white 
                         $ rectangleSolid size size
                         , translate x y
                         $ color black 
                         $ rectangleWire size size
                         , line [(x - 0.5, y - 0.5), (x + 0.5, y + 0.5)]
                         , line [(x - 0.5, y + 0.5), (x + 0.5, y - 0.5)]
                         ]
  | otherwise = blank
  where 
    size = 1;
    x = fromIntegral i;
    y = fromIntegral j


-- | Формируем список из пар (значение ячейки, координаты ячейки) для левой подсказки.
makeBotGrid :: Task -> [(Int, Coord)]
makeBotGrid side = zip (foldr (++) [] side) (makeBotAccordance numXY)
  where
    numX  = [[(negate x) .. (negate 1)] | x <- map length side]
    numXY = zip numX (reverse [(negate (length numX)) .. (negate 1)])

-- | [ (X, [0..Y]) ] .
-- | Список координат ячеек левой подсказки.
makeBotAccordance :: [([Int], Int)] -> [Coord]
makeBotAccordance = foldr (++) [] . map (\x -> zip (fst x) (repeat (snd x)))


-- | Формируем список из пар (значение ячейки, координаты ячейки) для верхней подсказки.
makeTopGrid :: Task -> [(Int, Coord)]
makeTopGrid side = zip (foldr (++) [] side) (makeTopAccordance numXY)
  where
    numY  = [[0 .. (x - 1)] | x <- map length side]
    numXY = zip [0 .. ((length numY) - 1)] numY    


-- | [ (X, [0..Y]) ] .
-- | Список координат ячеек верхней подсказки.
makeTopAccordance :: [(Int, [Int])] -> [Coord]
makeTopAccordance = foldr (++) [] . map (\x -> zip (repeat (fst x)) (reverse (snd x)))


-- | Рисует подсказки-ячейки (слева и сверху).
drawGrid :: Coord -> Int -> Int -> [(Int, Coord)] -> Picture
drawGrid (i, j) width height = translate (fromIntegral i) (fromIntegral j) 
                             . scale (fromIntegral width) (fromIntegral height) 
                             . pictures 
                             . map drawCell

-- | Рисует ячейку по заданным координатам и значению.
drawCell :: (Int, Coord) -> Picture
drawCell (n, (i, j))
  | (n == 0)  = pictures [
                          translate x y 
                         $ color white 
                         $ rectangleSolid size size
                         , 
                         translate x y 
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
                         $ text (show n)
                         ]
  where 
    size = 1;
    x = fromIntegral i;
    y = fromIntegral j
    

-- | Реакция на мышь и клавиатуру.
handleNonogram :: Event -> Nonogram -> Nonogram
handleNonogram (EventKey (MouseButton button) Down _ (x, y)) 
               start@Nonogram { solve = board 
                              , field = pic
                              , cols  = upSide
                              , rows  = leftSide
                              } 
  | and [ x > (fromIntegral left)
        , x < (fromIntegral right)
        , y > (fromIntegral bot)
        , y < (fromIntegral top)
        , button == LeftButton
        ]
    = Nonogram { solve = updateCell (i, j) changeD board, field = pic, cols = upSide, rows = leftSide } 
  | and [ x > (fromIntegral left)
        , x < (fromIntegral right)
        , y > (fromIntegral bot)
        , y < (fromIntegral top)
        , button == RightButton
        ]
    = Nonogram { solve = updateCell (i, j) changeU board, field = pic, cols = upSide, rows = leftSide }
  | otherwise
    = start
  where
    left = (div (negate windowWidth) 2) + cellSize * (maxLen leftSide)
    top = (div windowHeight 2) - cellSize * (maxLen upSide)
    right = left + cellSize * (length upSide)
    bot = top - cellSize * (length leftSide)
    i = floor (((fromIntegral top) - y) / (fromIntegral cellSize))
    j = floor ((x - (fromIntegral left)) / (fromIntegral cellSize))
handleNonogram (EventKey (Char key) Down _ _)
               Nonogram { solve = board 
                        , field = pic
                        , cols  = upSide
                        , rows  = leftSide
                        }
  | key == 'g' 
    = guessNonogram Nonogram { solve = board
                             , field = pic
                             , cols  = upSide
                             , rows  = leftSide
                             }
  | key == 'c' 
    = crossesNonogram Nonogram { solve = board
                               , field = pic
                               , cols  = upSide
                               , rows  = leftSide
                               }

  | key == 's' 
    = sidesNonogram Nonogram { solve = board
                             , field = pic
                             , cols  = upSide
                             , rows  = leftSide
                             }
  | key == 'a' 
    = lookForAccordance Nonogram { solve = board
                                 , field = pic
                                 , cols  = upSide
                                 , rows  = leftSide
                                 }
  | key == 'f' 
    = fillNonogram Nonogram { solve = board
                            , field = pic
                            , cols  = upSide
                            , rows  = leftSide
                            }
  | key == 'p' 
    = crossesNonogram Nonogram { solve = board
                                  , field = pic
                                  , cols  = upSide
                                  , rows  = leftSide
                                  }
  | key == 'o' 
    = autoSolve Nonogram { solve = board
                         , field = pic
                         , cols  = upSide
                         , rows  = leftSide
                         }
  | key == 'e' 
    = Nonogram { solve = emptySolve pic
               , field  = pic
               , cols = upSide
               , rows = leftSide
               }
  | otherwise
    = Nonogram { solve = board, field  = pic, cols = upSide, rows = leftSide}
handleNonogram _ a = a

updateNonogram :: Float -> Nonogram -> Nonogram
updateNonogram _ f = f
