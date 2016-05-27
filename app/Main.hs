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
import Data.Set

--Минное поле 4+5--
data CellState = Opened Int
               | Mine
               | Flag
--Минное поле 4+5--

--Минное поле 3--
type Field = Map Cell CellState
type Cell = (Int, Int)
--Минное поле 3--

--Минное поле 7--
type Mines = Set Cell
--Минное поле 7--

--Минное поле 2--
fieldSize@(fieldWidth, fieldHeight) = (15, 15) :: (Int, Int)
mineCount = 40 :: Int

startGame :: Field -> IO ()
startGame = undefined
--Минное поле 2--

--Минное поле 6--
createField :: Field
createField = Data.Map.empty
--Минное поле 6--



--Минное поле 1--
main :: IO ()
main = do
    let field = createField
    startGame field 
--Минное поле 1--
