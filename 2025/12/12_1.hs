import Data.List.Split (wordsBy, splitOn)

data Bin = Bin {w :: Int, h :: Int, shapeCounts :: [Int]} deriving (Show)

parseLine :: String -> Bin
parseLine s = Bin w h counts
    where
        (w:h:counts) = read <$> wordsBy (`elem` "x: ") s

fits :: Bin -> Bool
fits (Bin w h shapes) = (w `div` 3) * (h `div` 3) >= sum shapes

main :: IO ()
main = do
    input <- readFile "12.in"
    let
        parts = splitOn "\n\n" input
        bins = parseLine <$> lines (last parts)
    print $ length $ filter fits bins
