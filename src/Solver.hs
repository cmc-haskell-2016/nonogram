module Solver where

import Logic
import Data.List
--import Control.Exception

firstECell :: Int -> Field -> Coord
firstECell _ []     = error "bad solving"
firctECell n (x:xs) 
  | elem E x
    = (n, (head (findIndices isE x)))
  | otherwise
    = firstECell (n + 1) xs

--isCIn :: Field -> Field
--isCIn a = or . map (elem Conflict)

{-
isConflict :: Nonogram -> Nonogram
isConflict n@Nonogram { solve = board
                      , field = pic
                      , cols = top
                      , rows = left
                      }
  | isCIn board
    = autoSolve Nonogram { solve = updateCell whereTD (\x -> U) board
                         , field = pic
                         , cols = top
                         , rows = left
                         }
  | otherwise
    = id-}

autoSolve :: Nonogram -> Nonogram
autoSolve start@Nonogram { solve = board
                         , field = pic
                         , cols = top
                         , rows = left
                         }
  | pic == board
    = start
  | board == newBoard
     = autoSolve Nonogram { solve = updateCell firstE (\x -> TD) board
                          , field = pic
                          , cols = top
                          , rows = left
                          }
  | otherwise
    = autoSolve finish
    where
      firstE = firstECell 0 board
      finish@Nonogram { solve = newBoard
                      , field = pic
                      , cols = top
                      , rows = left
                      } = guessNonogram 
                        $ sidesNonogram 
                        $ crossesNonogram 
                        $ lookForAccordance 
                        $ fillNonogram start 
                        
guessNonogram :: Nonogram -> Nonogram
guessNonogram Nonogram { solve = board
                       , field = pic
                       , cols  = top
                       , rows  = left
                       } = Nonogram { solve = orField board
                                                      (orField (guessRow board left) (transpose (guessRow (transpose board) top)))
                                    , field = pic
                                    , cols = top
                                    , rows = left
                                    }

guessRow :: Field -> Task -> Field
guessRow board []   = board
guessRow board task = (guessBlocks (head board) (head task)) : (guessRow (tail board) (tail task))

guessBlocks :: [Cell] -> [Int] -> [Cell]
guessBlocks row []   = row
guessBlocks row task 
  | (head task) < space 
    = p ++ (guessBlocks q (tail task))
  | otherwise
    = a ++ b ++ [ c ] ++ (guessBlocks d (tail task))
  where
    lenRow  = length row
    lenTask = (foldr (+) 0 task) + (length task) - 1
    space   = lenRow - lenTask
    (a, x)  = splitAt space row
    (z, y)  = splitAt ((head task) - space) x
    b       = take (length z) (repeat D)
    (c:d)   = y
    (p, q)  = splitAt (head task) row

crossesNonogram :: Nonogram -> Nonogram
crossesNonogram Nonogram { solve = board
                         , field = pic
                         , cols  = top
                         , rows  = left
                         } = Nonogram { solve = orField (putCrosses left board) 
                                                        (transpose (putCrosses top (transpose board)))
                                      , field = pic
                                      , cols = top
                                      , rows = left
                                      }

putCrosses :: Task -> Field -> Field
putCrosses [] b = b
putCrosses a b  = (putCrossesLine (head a) (head b)) : (putCrosses (tail a) (tail b))

putCrossesLine :: [Int] -> [Cell] -> [Cell]
putCrossesLine a b 
  | a == (qLine b)
    = map (\x -> if (x == E) then U else x) b
  | otherwise
    = b

sidesNonogram :: Nonogram -> Nonogram
sidesNonogram Nonogram { solve = board
                       , field = pic
                       , cols  = top
                       , rows  = left
                       } = Nonogram { solve = orField (guessSides left board) 
                                                      (transpose (guessSides top (transpose board)))
                                    , field = pic
                                    , cols = top
                                    , rows = left
                                    }

guessSides :: Task -> Field -> Field
guessSides [] b = b
guessSides a b  = (guessLeftSide x (reverse (guessLeftSide (reverse x) (reverse y)))) 
                : (guessSides xs ys)
  where
    (x:xs) = a
    (y:ys) = b

guessLeftSide :: [Int] -> [Cell] -> [Cell]
guessLeftSide a b 
  | or [(length q) == 0, (length p) == 0, (last p) /= D]
    = b
  | (length y) == 0
    = p ++ x
  | otherwise
    = p ++ x ++ (U: (tail y))
  where
    p = takeWhile (not . isE) b
    q = dropWhile (not . isE) b
    indexOfCount = (length (filter (any isD) (group p))) - 1
    count = a !! indexOfCount
    realCount = count - (length (head (group (reverse p))))
    y = drop realCount q
    x = take realCount (repeat D)

guessRightSide :: Int -> [Cell] -> [Cell]
guessRightSide a b 
--  | (last b) == U 
--    = (guessRightSide a q) ++ p
  | (last b) == D
    = ((init y) ++ [U]) ++ x
  | otherwise
    = b
  where
    p = takeWhile
    y = take ((length b) - a) b
    x = take a (repeat D)

lookForAccordance :: Nonogram -> Nonogram
lookForAccordance Nonogram { solve = board
                           , field = pic
                           , cols  = top
                           , rows  = left
                           } = Nonogram { solve = orField (lookForRowAccordance left board) 
                                                          (transpose (lookForRowAccordance top (transpose board)))
                                        , field = pic
                                        , cols = top
                                        , rows = left
                                        }

lookForRowAccordance :: Task -> Field -> Field
lookForRowAccordance [] b = b
lookForRowAccordance a b  = (lookFor1Accordance (maximum (head a)) (group (head b))) : (lookForRowAccordance (tail a) (tail b))

lookFor1Accordance :: Int -> [[Cell]] -> [Cell]
lookFor1Accordance m a = foldr (++) [] (putSomeCrosses (findIndices (\x -> and [(length x) == m, any isD x]) a) a)

putSomeCrosses :: [Int] -> [[Cell]] -> [[Cell]]
putSomeCrosses [] a     = a
putSomeCrosses (x:xs) a = putSomeCrosses xs (put1Crosses x a)

put1Crosses :: Int -> [[Cell]] -> [[Cell]]
put1Crosses x a
  | and [x == 0, (length a) == 1]
    = a
  | x == 0
    = b : (U : (tail c)) : cs
  | x == ((length a) - 1)
    = (init p) ++ ((init (last p)) ++ [ U ]) : [ last a ]
  | otherwise
    = (init l) ++ ((init (last l)) ++ [ U ]) : m : (U : (tail n)) : ns
  where
    (b:(c:cs)) = a
    p = init a
    (l, m:n:ns) = splitAt x a


fillNonogram :: Nonogram -> Nonogram
fillNonogram Nonogram { solve = board
                      , field = pic
                      , cols  = top
                      , rows  = left
                      } = Nonogram { solve = orField (fillField left board) 
                                                     (transpose (fillField top (transpose board)))
                                   , field = pic
                                   , cols = top
                                   , rows = left
                                   }

fillField :: Task -> Field -> Field
fillField [] b = b
fillField a b  = (fillRow x y) : (fillField xs ys)
  where
    (x:xs) = a
    (y:ys) = b

fillRow :: [Int] -> [Cell] -> [Cell]
fillRow a b
  | (length a) == (length (filter (any isD) groupRow))
    = foldr (++) [] (fillBlocks a (findIndices (any isD) groupRow) groupRow)
  | otherwise
    = b
  where
    groupRow = group b
  
fillBlocks :: [Int] -> [Int] -> [[Cell]] -> [[Cell]]
fillBlocks [] _ a     = a
fillBlocks _ [] a     = a
fillBlocks (x:xs) (y:ys) a = fillBlocks xs ys (fill1Block x y a)

fill1Block :: Int -> Int -> [[Cell]] -> [[Cell]]
fill1Block n x a
  | or [x == 0, x == ((length a) - 1)]
    = a
  | and [i, any isU (a !! (x - 1))]
    = (take (x + 1) a) ++ (fillBlock (n - lenBlock) (drop (x + 1) a))
  | and [i, any isU (a !! (x + 1))]
    = (reverse (map reverse (fillBlock (n - lenBlock) (reverse (take x a))))) ++ (drop x a)
  | otherwise
    = a
  where
    i = n /= lenBlock
    lenBlock = length (a !! x)

fillBlock :: Int -> [[Cell]] -> [[Cell]]
fillBlock 0 a = a
fillBlock _ [] = []
fillBlock x (y:ys) = ((replicate m D) ++ (drop m y)) : (fillBlock (x - m) ys)
  where
    m = min x lenGroup
    lenGroup = length y


orField :: Field -> Field -> Field
orField a b = map orRow (zip a b)

orRow :: ([Cell], [Cell]) -> [Cell]
orRow (a, b) = map orCell (zip a b)

orCell :: (Cell, Cell) -> Cell
orCell (E, a) = a
orCell (a, E) = a
orCell (a, b) 
  | a == b
    = a
  | otherwise
    = Conflict

cellToChar :: Cell -> Char
cellToChar D = 'D'
cellToChar U = 'U'
cellToChar E = 'E'
