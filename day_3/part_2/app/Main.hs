module Main where
import qualified Data.Set as Set

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

calculateRoute :: [(Int, Int)] -> [(Int, Int)]
calculateRoute = scanl addTuples (0,0)

pickOddEntries :: [(Int, Int)] -> [(Int, Int)]
pickOddEntries xs = [x | (i, x) <- zip ([0..] :: [Int]) xs, odd i]

pickEvenEntries :: [(Int, Int)] -> [(Int, Int)]
pickEvenEntries xs = [x | (i, x) <- zip ([0..] :: [Int]) xs, even i]

countUnique :: [(Int, Int)] -> Int
countUnique list = Set.size $ Set.fromList list

main :: IO ()
main = do
    contents <- readFile "data.txt"
    putStrLn "EVEN AND ODD"
    let directions = map mapCharToDirection contents
        santasDirections = pickEvenEntries directions
        robotsDirections = (0, 0) : pickOddEntries directions
        santasVisits = calculateRoute santasDirections
        robotsVisits = calculateRoute robotsDirections
        allVisits = santasVisits ++ robotsVisits
        -- instead of this large intermediate list, we can do 
        -- allVisits = Set.union (Set.fromList santasVisits) (Set.fromList robotsVisits)
        -- countUnique = Set.size allVisits
        in print (countUnique allVisits)