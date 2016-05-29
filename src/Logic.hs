module Logic where

import Data.List

-- | Ячейка поля.
data Cell
  = E   -- ^ Пустая ячейка.
  | D   -- ^ ...
  | U
  deriving (Eq)

type Field = [[Cell]]

type Task = [[Int]]

data Nonogram = Nonogram
  { solve :: Field
  , field :: Field      -- ^ ...
  , cols  :: Task
  , rows  :: Task
  }

type Coord = (Int, Int)

makeNonogram :: Field -> Nonogram
makeNonogram pic = Nonogram (emptySolve pic) pic (qCols pic) (qRows pic)

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

