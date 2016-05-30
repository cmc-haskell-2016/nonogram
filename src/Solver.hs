{-# LANGUAGE RecordWildCards #-}

module Solver where

import Logic
import Data.List
import Data.Char


isConflict :: Nonogram -> Bool
isConflict Nonogram {solve = board} = or (map isConflict1 board)

isConflict1 :: [Cell] -> Bool
isConflict1 a = or (map (== Conflict) a)

chooseNonogram :: Nonogram -> Nonogram -> Nonogram
chooseNonogram a b
  | not (isConflict a)
    = a
  | otherwise
    = b

autoSolve :: Nonogram -> Nonogram
autoSolve start@Nonogram { solve = board
                         , field = pic
                         , cols = top
                         , rows = left
                         , assumptions = a
                         , frames = f
                         }
  | isConflict start
    = --autoSolve
      Nonogram { solve = updateCell (head a) (\x -> U) (head f)
               , field = pic
               , cols = top
               , rows = left
               , assumptions = (tail a)
               , frames = (tail f)
               } 
  | pic == board
    = start
  | board == newBoard
     = --(autoSolve 
     Nonogram { solve = updateCell (firstECell board) (\x -> D) board
                                          , field = pic
                                          , cols = top
                                          , rows = left
                                          , assumptions = ((firstECell board):a)
                                          , frames = (board:f)
                                          }
  | otherwise
    = --autoSolve 
    finish
    where
      finish@Nonogram { solve = newBoard
                      , field = pic
                      , cols = top
                      , rows = left
                      } = isOk
                        $ guessNonogram 
                        $ fillNonogram
                        $ sidesNonogram 
                        $ crossesNonogram 
                        $ lookForAccordance
                        $ putCrossesNonogram start 


--firstE = firstECell board

firstECell :: Field -> Coord
firstECell a = findE (zip (foldr (++) [] a) [(x, y) | x <- [0 .. (length a)-1], y <- [0 .. (length (head a)) - 1]])

--  | or (map isE (head a))
--    = (n, (head (findIndices isE (head a))))
--  | otherwise
--    = firstECell (n + 1) (tail a)
--
findE :: [(Cell, Coord)] -> Coord
findE a 
  | not ((filter (isE . fst) a) == [])
    = snd (head (filter (isE . fst) a))
  | otherwise
    = error (map cellToChar (fst (unzip a))) 


isOk :: Nonogram -> Nonogram
isOk Nonogram { solve = board
              , field = pic
              , cols  = top
              , rows = left
              , assumptions =  a
              , frames = f
              } = Nonogram { solve = orField (isOkField board left) (transpose (isOkField (transpose board) top))
                           , field = pic
                           , cols = top
                           , rows = left
              , assumptions = a
              , frames = f
                           }

isOkField :: Field -> Task -> Field
isOkField _ [] = []
isOkField (x:xs) (y:ys) = (isOkRow x y) : (isOkField xs ys)

isOkRow :: [Cell] -> [Int] -> [Cell]
isOkRow row task
  | or [(maximum task) < (maximum (0:(map length (filter (any isD) (group row)))))]
    = replicate (length row) Conflict
  | otherwise
    = row

guessNonogram :: Nonogram -> Nonogram
guessNonogram Nonogram { solve = board
                       , field = pic
                       , cols  = top
                       , rows  = left
                       , assumptions = a
                       , frames = f
                       } = Nonogram { solve = orField (guessRows board left) 
                                                      (guessCols board top )
                                    , field = pic
                                    , cols = top
                                    , rows = left
                                    , assumptions = a
                                    , frames = f
                                    }

guessCols :: Field -> Task -> Field
guessCols a b = transpose (guessRows (transpose a) b)

guessRows :: Field -> Task -> Field
guessRows  = zipWith guessBlocks 

guessBlocks :: [Cell] -> [Int] -> [Cell]
guessBlocks [] _     = []
guessBlocks row []   = row
guessBlocks row task 
  | (head row) == U
    = (takeWhile isU row) ++ (guessBlocks (dropWhile isU row) task)
  | and [any isD (head groupRow), (length groupRow) > 1, any isU (head (tail groupRow))] 
    = (takeWhile isD row) ++ (guessBlocks (dropWhile isD row) (tail task))
  | and [any isE (head groupRow), (length (head groupRow)) < (head task), any isU (head (tail groupRow))] 
    = (replicate (length (head groupRow)) U) ++ (guessBlocks (dropWhile isE row) task)
--  | and [any isE (head groupRow), (length (head groupRow)) < (head task), any isU (head (tail groupRow))] ошибка
--    = (replicate (length (head groupRow)) U) ++ (guessBlocks (dropWhile isE row) task)
--  | (head task) <= space -- если очередное задание невозможно отрисовать
--    = p ++ (guessBlocks q (tail task))
  | head task > space
    = a ++ b ++ [ c ] ++ (guessBlocks d (tail task))
  | otherwise 
    = row
  where
    lenRow  = length row
    lenTask = sum task + length task - 1
    space   = lenRow - lenTask
    (a, x)  = splitAt space row
    (z, y)  = splitAt ((head task) - space) x
    b       = replicate (head task - space) D
    (c:d)   = y
    (p, q)  = splitAt (head task) row
    groupRow = group row

crossesNonogram :: Nonogram -> Nonogram
crossesNonogram Nonogram { solve = board
                         , field = pic
                         , cols  = top
                         , rows  = left
              , assumptions = a
              , frames = f
                         } = Nonogram { solve = orField (putCrosses left board) 
                                                        (transpose (putCrosses top (transpose board)))
                                      , field = pic
                                      , cols = top
                                      , rows = left
              , assumptions = a
              , frames = f
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
              , assumptions = a
              , frames = f
                       } = Nonogram { solve = orField (guessSides left board) 
                                                      (transpose (guessSides top (transpose board)))
                                    , field = pic
                                    , cols = top
                                    , rows = left
              , assumptions = a
              , frames = f
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
  | or [(length q) == 0, (length p) == 0, (last p) /= D, indexOfCount > (length a)]
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

lookForAccordance :: Nonogram -> Nonogram
lookForAccordance Nonogram { solve = board
                           , field = pic
                           , cols  = top
                           , rows  = left
              , assumptions = a
              , frames = f
                           } = Nonogram { solve = orField (lookForRowAccordance left board) 
                                                          (transpose (lookForRowAccordance top (transpose board)))
                                        , field = pic
                                        , cols = top
                                        , rows = left
              , assumptions = a
              , frames = f
                                        }

lookForRowAccordance :: Task -> Field -> Field
lookForRowAccordance [] b = b
lookForRowAccordance a b  = (lookFor1Accordance (maximum (head a)) (group (head b))) : (lookForRowAccordance (tail a) (tail b))

lookFor1Accordance :: Int -> [[Cell]] -> [Cell]
lookFor1Accordance m a = concat (putSomeCrosses (findIndices (\x -> and [(length x) == m, any isD x]) a) a)

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
              , assumptions = a
              , frames = f
                      } = Nonogram { solve = orField (fillField left board) 
                                                     (transpose (fillField top (transpose board)))
                                   , field = pic
                                   , cols = top
                                   , rows = left
              , assumptions = a
              , frames = f
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

putCrossesNonogram :: Nonogram -> Nonogram
putCrossesNonogram Nonogram { solve = board
                            , field = pic
                            , cols  = top
                            , rows  = left
                            , assumptions = a
                            , frames = f
                            } = Nonogram { solve = orField 
                            (putCrossesField left board) 
                                                           (transpose (putCrossesField top (transpose board)))
                                         , field = pic
                                         , cols = top
                                         , rows = left
              , assumptions = a
              , frames = f
                                         }

putCrossesField :: Task -> Field -> Field
putCrossesField [] b = b
putCrossesField a b  = (putCrossesRow x y) : (putCrossesField xs ys)
  where
    (x:xs) = a
    (y:ys) = b

putCrossesRow :: [Int] -> [Cell] -> [Cell]
putCrossesRow _ [] = []
putCrossesRow [] b = b
putCrossesRow a b
  | or [(length groupB) <= 2, (length a) > 1]
    = b
  | (head b) == D
    = (takeWhile isD b) ++ (putCrossesRow (tail a) (dropWhile isD b))
  | (head b) == U
    = (takeWhile isU b) ++ (putCrossesRow a (dropWhile isU b))
  | and [any isE x, any isD y, any isE z]
    =  (replicate xU U) 
    ++ (replicate numE E) 
    ++ y 
    ++ (replicate numE E) 
    ++ (replicate zU U)
    ++ (putCrossesRow (tail a) (foldr (++) [] zs))
  | otherwise
    = x ++ (putCrossesRow a (foldr (++) [] (y:z:zs)))
  where
    groupB      = group b
    (x:y:z:zs) = groupB
    numE = min ((head a) - (length y)) (length x)
    xU = (length x) - numE
    zU = (length z) - numE


orField :: Field -> Field -> Field
orField = zipWith orRow

orRow :: [Cell] -> [Cell] -> [Cell]
orRow = zipWith orCell

orCell :: Cell -> Cell -> Cell
orCell _ Conflict = Conflict
orCell Conflict _ = Conflict
orCell E a = a
orCell a E = a
orCell a b 
  | a == b
    = a
  | otherwise
    = Conflict

cellToChar :: Cell -> Char
cellToChar D = 'D'
cellToChar U = 'U'
cellToChar E = 'E'
