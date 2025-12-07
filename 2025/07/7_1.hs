import qualified Data.Set as Set
import Data.List

caretIndices :: String -> [Int]
caretIndices str = [i | (i, c) <- zip [0..] str, c == '^']

traverseTree :: [Set.Set Int] -> Set.Set Int -> Int -> Int
traverseTree [] _ splitCount = splitCount
traverseTree (rowSet:rows) activeBeams splitCount =
    let (newBeams, splitsThisRow) =
            Set.foldr
                (\beam (acc, sCount) ->
                    if beam `Set.member` rowSet
                        then (Set.insert (beam-1) $ Set.insert (beam+1) acc, sCount + 1)
                        else (Set.insert beam acc, sCount)
                )
                (Set.empty, 0)
                activeBeams
    in traverseTree rows newBeams (splitCount + splitsThisRow)

main :: IO ()
main = do
    input <- readFile "7.in"
    let ls = lines input
        sCol = case findIndex (=='S') (head ls) of
                 Just i -> i
                 Nothing -> error "No S found"
        beamTree = filter (any (/= '.')) (tail ls)
        idxSets = map (Set.fromList . caretIndices) beamTree
    print $ traverseTree idxSets (Set.singleton sCol) 0
