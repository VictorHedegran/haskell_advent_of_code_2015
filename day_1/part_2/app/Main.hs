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
    
findBasementIndex :: [Int] -> Int -> Int -> Int
findBasementIndex [] _ _ = -1
findBasementIndex (i:is) index floorsum 
    | floorsum + i == -1 = index + 1 
    | otherwise = findBasementIndex is (index + 1) (floorsum + i)

main :: IO ()
main = do
    contents <- readFile "data.txt"
    print (findBasementIndex (map mapCharToNumber contents) 0 0 )
