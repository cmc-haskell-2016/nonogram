module Logic where

import Data.List

-- | Ячейка поля.
data Cell
  = E   -- ^ Пустая ячейка.
  | D   -- ^ ...
  | U
  | Conflict
  deriving (Eq, Show)

type Field = [[Cell]]

type Task = [[Int]]

data Nonogram = Nonogram
  { solve       :: Field
  , field       :: Field      -- ^ ...
  , cols        :: Task
  , rows        :: Task
  , assumptions :: [Coord]
  , frames      :: [Field]
  }

type Coord = (Int, Int)

makeNonogram :: Field -> Nonogram
makeNonogram pic = Nonogram (emptySolve pic) pic (qCols pic) (qRows pic) [] []

emptySolve :: Field -> Field
emptySolve pic = take (length pic) (repeat (take (length (head pic)) (repeat E)))

qRows :: Field -> Task
qRows = map qLine 

qCols :: Field -> Task
qCols = map qLine . transpose

qLine :: [Cell] -> [Int]
qLine = map length . filter (any isD) . group

isD :: Cell -> Bool
isD D = True
isD _ = False

isU :: Cell -> Bool
isU U = True
isU _ = False

isE :: Cell -> Bool
isE E = True
isE _ = False

changeD :: Cell -> Cell
changeD D = E
changeD _ = D

changeU :: Cell -> Cell
changeU U = E
changeU _ = U

updateCell :: Coord -> (Cell -> Cell) -> Field -> Field
updateCell (i, j) f board = updateElem i (updateElem j f) board

updateElem :: Int -> (a -> a) -> [a] -> [a]
updateElem n f xs = ys ++ [f z] ++ zs
  where
    (ys, z:zs) = splitAt n xs

readField :: String -> Field
readField = map (map readCell) . lines

readCell :: Char -> Cell
readCell '.'  = U
readCell 'O'  = D
readCell _    = error "Invalid nonogram file"

-- | Загрузить головоломку из файла.
importNonogram :: FilePath -> IO Nonogram
importNonogram path = do
  contents <- readFile path
  return (makeNonogram (readField contents))

windowWidth :: Int
windowWidth = 500

windowHeight :: Int
windowHeight = 500

cellSize :: Int
cellSize = 20

{-map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map _ [] _ = []
map _ _ [] = []
map2 f a b = (f x y) : (map2 f xs ys)
  where
    (x:xs) = a
    (y:ys) = b-}
