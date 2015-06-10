module Main where

import System.IO


main = do
    putStrLn "Digite algo"
    line <- getLine
    putStrLn $ "massa, valeu por digitar " ++ line
