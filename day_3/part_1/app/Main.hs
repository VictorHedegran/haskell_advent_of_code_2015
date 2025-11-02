module Main where

north :: (Int, Int)
north = (0, 1)

south :: (Int, Int)
south = (0, -1)

east :: (Int, Int)
east = (1, 0)

west :: (Int, Int)
west = (-1, 0)

addTuples ::  (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

mapCharToDirection :: Char -> (Int, Int)
mapCharToDirection '>' = east
mapCharToDirection '<' = west
mapCharToDirection '^' = north
mapCharToDirection 'v' = south
mapCharToDirection _ = (0, 0)

calculateRoute :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
calculateRoute [] _ = [(0, 0)]
calculateRoute (x:xs) previousLocation =
    let newHouse = addTuples x previousLocation 
    in newHouse : calculateRoute xs newHouse

-- possible to substitute calculateRoute with 
-- scanl addTuples (0,0) [directions]

addIfUnique :: (Eq a, Eq b) => (a, b) -> [(a, b)] -> Maybe (a, b)
addIfUnique t1 [] = Just t1
addIfUnique t1 (t:ts) 
    | t1 == t = Nothing
    | otherwise = addIfUnique t1 ts

reduceByUnique :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
reduceByUnique [] y = y
reduceByUnique (x:xs) [] = reduceByUnique xs [x]
reduceByUnique (x:xs) y = reduceByUnique xs (case addIfUnique x y of
    Just t  -> t : y
    Nothing -> y)

-- converting list to set removes duplicates! 
-- uniqueVisitedCount positions = Set.size $ Set.fromList positions

main :: IO ()
main = do
    contents <- readFile "data.txt"
    print (length (reduceByUnique (calculateRoute (map mapCharToDirection contents) (0, 0))  []))
