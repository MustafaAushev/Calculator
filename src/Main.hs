module Main where

import Calculator

import System.Environment

main :: IO()
main = do 
    args <- getArgs
    parseArgs args
    
parseArgs :: [String] -> IO()
parseArgs []        = calculating
parseArgs y@(x:_)   = do
    case x of
         "--help"       -> usage
         "--version"    -> version
         _              -> calculating
         
calculating :: IO()
calculating = do
    putStrLn $ "Введите выражение для подсчёта без знака =, например: 2+2"
    str <- getLine
    putStr str
    putStr "="
    print ( calculator str )

usage :: IO()
usage = putStrLn usageStr

usageStr :: String
usageStr =  concat[ "Использование:\n"                                                  ,
                    "   Введите любое выражение, кроме \"--help\" и \"--version\", \n"     ,
                    "   и калькулятор посчитает его     .\n"           ]
    
version :: IO()
version = putStrLn versionStr

versionStr :: String
versionStr =    "Calculator, version 1.1."
