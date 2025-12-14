import Data.List.Split (splitOn)
import Data.List (foldl')
import Data.Bits ((.|.), xor, shiftL)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

data Machine = Machine { indicators :: Int, buttons :: [Int] } deriving (Show,Eq)

stripBrackets :: String -> String
stripBrackets = init . tail

parseTuple :: String -> [Int]
parseTuple s =
    case stripBrackets s of
        "" -> []
        xs -> read ("[" ++ xs ++ "]")

parseIndicators :: String -> Int
parseIndicators = foldl' (\acc c -> acc * 2 + bit c) 0 . reverse
  where
    bit '#' = 1
    bit '.' = 0
    bit _   = error "invalid character"

parseButtons :: [String] -> [Int]
parseButtons = map (foldl' (\acc i -> acc .|. (1 `shiftL` i)) 0 . parseTuple)

parseMachine :: String -> Machine
parseMachine input =
    let
        parts = splitOn " " input
        buttonPart =  init (tail parts)
        indicators = parseIndicators . stripBrackets $ head parts
        buttons = parseButtons buttonPart
    in Machine indicators buttons

bfs :: Machine -> [Int]
bfs (Machine indicators buttons) = go (Seq.singleton (indicators, [])) Set.empty
    where
        go Seq.Empty _ = []
        go ((current, used) Seq.:<| queue) visited
            | current == 0 = used
            | current `Set.member` visited = go queue visited
            | otherwise = 
                let 
                    visited' = Set.insert current visited
                    nextStates = [(current `xor` b, used ++ [b]) | b <- buttons]
                in go (queue Seq.>< Seq.fromList nextStates) visited' 

main :: IO()
main = do
    input <- lines <$> readFile "10.in"
    let machines =  parseMachine <$> input
        shortestSolutions = bfs <$> machines
    print $ sum $ length <$> shortestSolutions