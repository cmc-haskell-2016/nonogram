--Sources:
--https://habrahabr.ru/post/249803/
--Программы коллег на git'е

--EasyGit:
--обновление репозитория
--git add <каталог или файл, который хотел бы добавить>
--git commit -m "<комментарий к тому что добавил>"
--git push
--создание ветки
--git branch <имя создаваемой ветки>
--git branch -v ;список созданных веток
--git checkout <имя ветки, к которой переходим>
--git config --global push.default simple
--а дальше делаешь то что для обновления
-- + еще одну команду что скажет терминал

--Actions:
--sudo apt-get update
--sudo apt-get install ghc
--sudo apt-get install stack
--sudo apt-get install git
--sudo apt-get install cabal-install
--sudo apt-get install freeglut3 freeglut3-dev
--sudo apt-get install llvm
--sudo apt-get install libssl-dev
--cabal update
--cabal install gloss-examples
--cabal install containers
--cabal install random-shuffle
--cabal install randomgen
--действия с git'a

module Main where

import Lib
import Data.Map
import Graphics.Gloss.Interface.Pure.Game

--создали константу размера поля как кортеж из ширины и высоты поля
fieldSize@(fieldWidth, fieldHeight) = (10, 10) :: (Int, Int)
--размер клетки поля
cellSize = 24

--состояния клетки поля
                 --не заполнено
data CellState = Empty
                 --закрашено
               | X
                 --не закрашено
               | O

data GameState = GS 
     { field :: Field
     , plot :: Field
     }

--поле есть соответствие между ячейками и их состоянием
type Field = Map Cell CellState
--ячейка есть пара целых чисел
type Cell = (Int, Int)


createField :: Field
createField = Data.Map.empty

startGame :: Field -> IO ()
startGame f = play (InWindow 
                    "Nonogram.pro" 
                    (both (* (round cellSize)) fieldSize)
                    (240, 160)) 
                   (greyN 0.25) 
                   30 
                   f 
                   renderer 
                   handler 
                   updater

updater _ = id

handler :: Event -> Field -> Field
handler (EventKey (MouseButton LeftButton) Down _ (x, y)) 
        f@Field { = f
        _ f = f
        
  

renderer :: Field -> Picture
renderer _ = pictures [uncurry
                       translate
                       (cellsSize (x, y))
                     $ color
                       white
                     $ rectangleSolid
                       cellSize
                       cellSize
                     | x <- [0 .. fieldWidth - 1],
                       y <- [0 .. fieldHeight - 1]]
                      

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)


cellsSize = both ((* cellSize) . fromIntegral) 

main :: IO ()
main = do
    let field = createField --создаем поле
    startGame field         --запускаем игру на созданном поле
