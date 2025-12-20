import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (foldl')
import Data.List.Split (splitOn)

data Machine = Machine
  { goalVec   :: Vector Int
  , coeffsVec :: [Vector Int]
  } deriving Show

stripParens :: String -> String
stripParens = init . tail

parseTuple :: String -> [Int]
parseTuple s =
    case stripParens s of
        "" -> []
        xs -> map read $ splitOn "," xs

tupleToIndicator :: Int -> [Int] -> Vector Int
tupleToIndicator len xs =
    let s = Set.fromList xs
    in V.generate len (\i -> if Set.member i s then 1 else 0)

parseLine :: String -> Machine
parseLine line =
    let toks = words line
        (_:middle) = toks
        (coeffTokens, [goalToken]) = splitAt (length middle - 1) middle
        goal = V.fromList $ parseTuple goalToken
        coeffs = map (tupleToIndicator (V.length goal) . parseTuple) coeffTokens
    in Machine goal coeffs

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations count (x:xs) = map (x:) (combinations (count-1) xs) ++ combinations count xs

patterns :: [Vector Int] -> Map (Vector Int) Int 
patterns buttons = findCombinations 0 Map.empty
    where 
        numButtons = length buttons
        numVars = V.length (head buttons)
        indices = [0..numButtons-1]
        zeroVec = V.replicate numVars 0
        addVec = V.zipWith (+)

        findCombinations :: Int -> Map (Vector Int) Int -> Map (Vector Int) Int
        findCombinations patternLen patternMap
            | patternLen > numButtons = patternMap
            | otherwise =
                let 
                    combs = combinations patternLen indices
                    patternMap' = foldl' (\accMap idxs ->
                            let res = foldl' addVec zeroVec (map (buttons !!) idxs)
                            in Map.insertWith (\_ old -> old) res patternLen accMap
                        )
                        patternMap combs
                in findCombinations (patternLen + 1) patternMap'

solveSingle :: [Vector Int] -> Vector Int -> Int
solveSingle buttons initGoal = fst $ minPressesMemo Map.empty initGoal
    where
        combs = patterns buttons
        reachedGoal = V.all (== 0) 
        inf = 1000000

        minPressesMemo :: Map (Vector Int) Int -> Vector Int -> (Int, Map (Vector Int) Int)
        minPressesMemo memo goal
            | reachedGoal goal = (0, Map.insert goal 0 memo)
            | Just v <- Map.lookup goal memo = (v, memo)
            | otherwise =
                let 
                    (best, memo') = foldl' (\(curBest, curMemo) (pat, cost) ->
                                                if V.and $ V.zipWith (\p g -> p <= g && p `mod` 2 == g `mod` 2) pat goal
                                                then 
                                                    let
                                                        newGoal = V.zipWith (\i j -> (j - i) `div` 2) pat goal
                                                        (subAns, nextMemo) = minPressesMemo curMemo newGoal
                                                    in (min curBest (cost + 2*subAns), nextMemo)
                                                else (curBest, curMemo)
                                            ) (inf, memo) (Map.toList combs)
                    memoFinal = Map.insert goal best memo'
                in (best, memoFinal)

processLine :: String -> Int
processLine line = 
    let Machine goal coeffs = parseLine line
    in solveSingle coeffs goal

main :: IO ()
main = do
    contents <- readFile "10.in"
    let ls = lines contents
        results = map processLine ls
    print $ sum results
