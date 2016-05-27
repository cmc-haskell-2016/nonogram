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
--sido apt-get install ghci
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

fieldSize@(fieldWidth, fieldHeight) = (15, 15) :: (Int, Int) --создали константу размера поля как кортеж из ширины и высоты поля
cellSize = 24

data CellState = Empty
               | X
               | O

type Field = Map Cell CellState
type Cell = (Int, Int)


createField :: Field
createField = Data.Map.empty

startGame :: Field -> IO ()
startGame field = play (InWindow "Nonogram.pro" (240, 160) (10, 10)) 
                       yellow 
                       30 
                       field 
                       renderer 
                       handler 
                       updater

updater _ = id

handler _ = id

renderer _ = pictures [uncurry translate (cellToScreen (x, y)) $ color white $ rectangleSolid cellSize cellSize
                      | x <- [0 .. fieldWidth - 1], y <- [0 .. fieldHeight - 1]]

cellToScreen = both ((* cellSize) . fromIntegral)

main :: IO ()
main = do
    let field = createField --создаем поле
    startGame field         --запускаем игру на созданном поле
