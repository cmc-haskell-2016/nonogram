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
  , field :: Field      -- ^ Картинка.
  , cols  :: Task
  , rows  :: Task
  }

type Coord = (Int, Int) -- ^ Координата ячейки.

-- | Начало игры.
makeNonogram :: Field -> Nonogram
makeNonogram pic = Nonogram (emptySolve pic) pic (qCols pic) (qRows pic)


-- | Создаём пустое поле из E.
emptySolve :: Field -> Field
emptySolve pic = take (length pic) (repeat (take (length (head pic)) (repeat E)))

-- | Создаем подсказку для строк.
qRows :: Field -> Task
qRows = map qLine 

-- | Создаем подсказку для столбцов.
qCols :: Field -> Task
qCols = map qLine . transpose

-- | Считаем кол-во заполненных ячеек для подсказки.
qLine :: [Cell] -> [Int]
qLine = map length . filter (any isD) . group


-- | Идентификаторы.
-- | Проверка, является ли ячейка D.
isD :: Cell -> Bool
isD D = True
isD _ = False

-- | Проверка, является ли ячейка U.
isU :: Cell -> Bool
isU U = True
isU _ = False

-- | Проверка, является ли ячейка E.
isE :: Cell -> Bool
isE E = True
isE _ = False


-- | Модификаторы.
-- | Если значение отлично от D, то записать в ячейку D, иначе - очистить клетку (Е).
changeD :: Cell -> Cell
changeD D = E
changeD _ = D

-- | Если значение отлично от U, то записать в ячейку U, иначе - очистить клетку (Е).
changeU :: Cell -> Cell
changeU U = E
changeU _ = U


-- | Изменить значение в (i, j)-ячейке.  
updateCell :: Coord -> (Cell -> Cell) -> Field -> Field
updateCell (i, j) f board = updateElem i (updateElem j f) board

-- | Изменить n-ый элемент списка, применив f.
updateElem :: Int -> (a -> a) -> [a] -> [a]
updateElem n f xs = ys ++ [f z] ++ zs
  where
    (ys, z:zs) = splitAt n xs

-- | Считывание поля из файла.
-- | Разбиваем строку (текст файла) на отдельные строки,
-- | первый map к списку строк,
-- | второй map к элементам строк, тем самым
-- | определяем тип каждой ячейки.
readField :: String -> Field
readField = map (map readCell) . lines

-- | Сопоставление символу тип клетки.
readCell :: Char -> Cell
readCell '.'  = U
readCell 'O'  = D
readCell _    = error "Invalid nonogram file"

-- | Вызывается из main.
-- | Загрузить головоломку из файла.
importNonogram :: FilePath -> IO Nonogram
importNonogram path = do
  contents <- readFile path
  return (makeNonogram (readField contents))


-- | Размеры окна.
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
