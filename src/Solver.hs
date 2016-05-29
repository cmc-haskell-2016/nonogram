module Solver where

import Logic
import Data.List

guessNonogram :: Nonogram -> Nonogram
guessNonogram Nonogram { solve = board
                       , field = pic
                       , cols  = top
                       , rows  = left
                       } = Nonogram { solve = orField board
                                                      (orField (guessSide board left) (transpose (guessSide (transpose board) top)))
                                    , field = pic
                                    , cols = top
                                    , rows = left
                                    }

guessSide :: Field -> Task -> Field
guessSide board []   = board
guessSide board task = (guessBlocks (head board) (head task)) : (guessSide (tail board) (tail task))

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

orField :: Field -> Field -> Field
orField a b = map orRow (zip a b)

orRow :: ([Cell], [Cell]) -> [Cell]
orRow (a, b) = map orCell (zip a b)

orCell :: (Cell, Cell) -> Cell
orCell (_, D) = D
orCell (D, _) = D
orCell (E, U) = U
orCell (U, E) = U
orCell (_, _) = E

crossesNonogram :: Nonogram -> Nonogram
crossesNonogram Nonogram { solve = board
                         , field = pic
                         , cols  = top
                         , rows  = left
                         } = Nonogram { solve = putCrosses left board
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

{-qLine :: [Cell] -> [Int]
qLine = map length . filter (any isD) . group

isD :: Cell -> Bool
isD D = True-}
