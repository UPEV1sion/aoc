import Data.List.Split (splitOn)
import Data.List (nub)

type Range = (Int, Int)
type RangeSet = [Range]

insertRange :: Range -> RangeSet -> RangeSet
insertRange r [] = [r]
insertRange r (x:xs)
    | snd r < fst x = r:x:xs
    | snd x < fst r = x:insertRange r xs
    | otherwise = insertRange (min (fst r) (fst x), max (snd r) (snd x)) xs

rangeExists :: Range -> RangeSet -> Bool
rangeExists r = any (\(s,e) -> fst r < e && snd r > s)

parseRange :: String -> Range
parseRange s =
    let [start, stop] = map read $ splitOn "-" s
    in (start, stop)

getRangeCount :: RangeSet -> Int
getRangeCount = foldr (\ x -> (+) (snd x - fst x + 1)) 0

main :: IO()
main = do
    input <- readFile "5.in"
    let [rangePart, _] = splitOn "\n\n" input
        ranges = map parseRange $ lines rangePart
        merged = foldr insertRange [] ranges
    print $ getRangeCount merged