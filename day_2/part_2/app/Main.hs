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

calculateRibbon :: [Int] -> Int
calculateRibbon [x, y, z] = x + x + y + y + x * y * z
calculateRibbon _ = error "Expect 3 values"

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    quickSort (filter (<= x) xs)
    ++ [x] ++
    quickSort (filter (> x) xs)

convertListToInt :: [String] -> [Int]
convertListToInt [] = []
convertListToInt (x:xs) =  convertListToInt xs ++ [read x :: Int]

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

main :: IO ()
main = do
    contents <- readFile "data.txt"
    let ls = lines contents  -- splits by newline
    let split = map splitByX ls
    mapM_ (print . quickSort . convertListToInt) split
    putStrLn "Result ft of materials"
    print (sumList (map (calculateRibbon . quickSort . convertListToInt) split))
