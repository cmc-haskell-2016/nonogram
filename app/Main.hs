--Sources:
--https://habrahabr.ru/post/249803/
--Программы коллег на git'е

--Actions:
--sudo apt-get update
--sido apt-get install ghci
--sudo apt-get install stack
--sudo apt-get install git
--действия с git'a


module Main where

import Lib

main :: IO ()
main = do
       s <- getLine
       putStrLn (someFunc s)
