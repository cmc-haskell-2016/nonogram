module Logic where

import Data.List

-- | Ячейка поля.
data Cell
  = E	-- ^ Пустая ячейка.
  | D	-- ^ ...
  | U
  deriving (Eq)

type Field = [[Cell]]

type Question = [[Int]]

data Nonogram = Nonogram
  { field :: Field	-- ^ ...
  , cols  :: Question
  , rows  :: Question
  }

type Coord = (Int, Int)

changeD :: Cell -> Cell
changeD D = E
changeD _ = D

changeU :: Cell -> Cell
changeU U = E
changeU _ = U

updateCell :: Coord -> (Cell -> Cell) -> Field -> Field
updateCell (i, j) f field = updateElem i (updateElem j f) field

updateElem :: Int -> (a -> a) -> [a] -> [a]
updateElem n f xs = ys ++ [f z] ++ zs
  where
    (ys, z:zs) = splitAt n xs


makeNonogram :: Field -> Nonogram
makeNonogram field = Nonogram field (qCols field) (qRows field)

qRows :: Field -> Question
qRows = map qLine 

qCols :: Field -> Question
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
readCell _    = error "invalid nongram file"

-- | Загрузить головоломку из файла.
importNonogram :: FilePath -> IO Nonogram
importNonogram path = do
  contents <- readFile path
  return (makeNonogram (readField contents))

