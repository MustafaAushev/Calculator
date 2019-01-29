module Main where

import Calculator

main :: IO()
main = do 
    str <- getLine
    print ( calculator str )
