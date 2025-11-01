{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldl" #-}
{-# HLINT ignore "Use foldr" #-}
module Main where

splitBy :: Char -> String -> [String]
splitBy _ "" = []
splitBy delim str = 
    let (first, rest) = span (/=delim) str
    in first : case rest of
        [] -> []
        (_:xs) -> splitBy delim xs

splitByX :: String -> [String]
splitByX = splitBy 'x'

calculateBox :: [Int] -> Int
calculateBox [x, y, z] = 2 * x * y +2 * y * z + 2 * x * z + minimum [x * y, y * z, x * z]
calculateBox _ = error "Expect 3 values"

convertListToInt :: [String] -> [Int]
convertListToInt [] = []
convertListToInt (x:xs) =  convertListToInt xs ++ [read x :: Int]

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

main :: IO ()
main = do
    contents <- getContents
    let ls = lines contents  -- splits by newline
    let split = map splitByX ls
    mapM_ (print . calculateBox . convertListToInt) split
    putStrLn "Result ft of materials"
    print (sumList (map (calculateBox . convertListToInt) split))
