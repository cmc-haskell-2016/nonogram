--Sources:
--https://habrahabr.ru/post/249803/
--Программы коллег на git'е


module Main where

import Lib

main :: IO ()
main = do
       s <- getLine
       putStrLn (someFunc s)
