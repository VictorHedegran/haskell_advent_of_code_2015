{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldl" #-}
{-# HLINT ignore "Use foldr" #-}
module Main where

sumList :: [Int] -> Int
sumList [] = 0
sumList (i:is) = i + sumList is

mapCharToNumber :: Char -> Int
mapCharToNumber '(' = 1
mapCharToNumber ')' = -1
mapCharToNumber _ = 0


interpretNumbersFromInput :: String -> [Int] -> [Int]
interpretNumbersFromInput [] is = is
interpretNumbersFromInput (c:cs) is = interpretNumbersFromInput cs (mapCharToNumber c : is)
    

main :: IO ()
main = do
    contents <- readFile "data.txt"
    print (sumList (interpretNumbersFromInput contents []))
