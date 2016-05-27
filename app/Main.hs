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
--cabal update
--cabal install gloss-examples
--действия с git'a

module Main where

import Lib

main :: IO ()
main = do
       s <- getLine
       putStrLn (someFunc s)
