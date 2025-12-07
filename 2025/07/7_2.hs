import qualified Data.Map.Strict as Map
import Data.List
import Data.Maybe (fromMaybe)
import Data.ByteString (count)

caretIndices :: String -> [Int]
caretIndices str = [i | (i, c) <- zip [0..] str, c == '^']

traverseTree :: [ [Int] ] -> Map.Map Int Int -> Map.Map Int Int
traverseTree [] activeBeams = activeBeams
traverseTree (rowCols:rows) activeBeams =
    let rowSet = Map.fromList [(i, ()) | i <- rowCols]
        newBeams = Map.foldrWithKey 
            (\beam count accMap -> 
                if beam `Map.member` rowSet
                    then 
                        Map.insertWith (+) (beam-1) count $
                        Map.insertWith (+) (beam+1) count accMap
                    else 
                        Map.insertWith (+) beam count accMap
            )
            Map.empty
            activeBeams
    in traverseTree rows newBeams

main :: IO ()
main = do
    input <- readFile "7.in"
    let ls = lines input
        sCol = case findIndex (=='S') (head ls) of
                 Just i -> i
                 Nothing -> error "No S found"
        beamTree = filter (any (/= '.')) (tail ls)
        idxLists = map caretIndices beamTree
        initialBeams = Map.singleton sCol 1
        finalBeams = traverseTree idxLists initialBeams
    print $ sum (Map.elems finalBeams)