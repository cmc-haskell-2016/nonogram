module Logic where

import Data.List

-- | Ячейка поля.
data Cell
  = E   -- ^ Пустая ячейка.
  | D   -- ^ Закрашенная ячейка.
  | U   -- ^ Крестик.
  | Conflict  -- ^ Неверная отметка.
  deriving (Eq) 

type Field = [[Cell]]

type Task = [[Int]]

data Nonogram = Nonogram
  { solve :: Field      -- ^ Текущее поле.
  , field :: Field      -- ^ Исходная картинка.
  , cols  :: Task
  , rows  :: Task
  }

type Coord = (Int, Int) -- ^ Координаты ячейки.

-- | Создание nonogram по полю для начала игры.
makeNonogram :: Field -> Nonogram
makeNonogram pic = Nonogram (emptySolve pic) pic (qCols pic) (qRows pic)


-- | Создаём пустое поле.
emptySolve :: Field -> Field
emptySolve pic = take (length pic) (repeat (take (length (head pic)) (repeat E)))

-- | Создаем подсказку для строк.
qRows :: Field -> Task
qRows = map qLine 

-- | Создаем подсказку для столбцов.
qCols :: Field -> Task
qCols = map qLine . transpose

-- | Формируем подсказку по линии (столбцу или строке).
qLine :: [Cell] -> [Int]
qLine = map length . filter (any isD) . group


-- | Проверка, закрашена ли ячейка.
isD :: Cell -> Bool
isD D = True
isD _ = False

-- | Проверка, помечена ли ячейка крестиком.
isU :: Cell -> Bool
isU U = True
isU _ = False

-- | Проверка, пуста ли ячейка.
isE :: Cell -> Bool
isE E = True
isE _ = False


-- | Если ячейка закрашена, очистить её, иначе - закрасить.
changeD :: Cell -> Cell
changeD D = E
changeD _ = D

-- | Если ячейка помечена кретиком, очистить её, иначе - пометить.
changeU :: Cell -> Cell
changeU U = E
changeU _ = U


-- | Изменить значение в ячейке с координатами (i, j).  
updateCell :: Coord -> (Cell -> Cell) -> Field -> Field
updateCell (i, j) f board = updateElem i (updateElem j f) board

-- | Изменить n-ый элемент списка, применив f.
updateElem :: Int -> (a -> a) -> [a] -> [a]
updateElem n f xs = ys ++ [f z] ++ zs
  where
    (ys, z:zs) = splitAt n xs

-- | Считывание и формирование поля по картинке из файла.
readField :: String -> Field
readField = map (map readCell) . lines

-- | Сопоставление символу типа клетки.
readCell :: Char -> Cell
readCell '.'  = U
readCell 'O'  = D
readCell _    = error "Invalid nonogram file"


-- | Загрузить головоломку из файла.
importNonogram :: FilePath -> IO Nonogram
importNonogram path = do
  contents <- readFile path
  return (makeNonogram (readField contents))


-- | Ширина окна.
windowWidth :: Int
windowWidth = 500

-- | Высота окна.
windowHeight :: Int
windowHeight = 500

-- | Размер ячеек.
cellSize :: Int
cellSize = 20

