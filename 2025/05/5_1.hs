import Data.List.Split (splitOn)

type Range = (Integer, Integer)

parseRange :: String -> Range
parseRange s =
    let [start, stop] = map read $ splitOn "-" s
    in (start, stop)

inRange :: Integer -> Range -> Bool
inRange x (start, stop) = x >= start && x <= stop

main :: IO()
main = do 
    input <- readFile "5.in"
    let [rangePart, idPart] = splitOn "\n\n" input
        ranges = map parseRange $ lines rangePart
        ids = map read $ lines idPart
        count = length [i | i <- ids, any (inRange i) ranges]
    print count