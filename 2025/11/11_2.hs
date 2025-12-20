import qualified Data.Map as Map
import Data.List (foldl')

parseLine :: String -> (String, [String])
parseLine s = 
    let
        nodes = words s
        parent = head nodes
        successors = tail nodes
    in (init parent, successors)


dfs :: Map.Map String [String] -> Int
dfs graph = fst $ countPaths "svr" False False Map.empty
    where
        countPaths :: String -> Bool -> Bool -> Map.Map (String, Bool, Bool) Int -> (Int, Map.Map (String, Bool, Bool) Int)
        countPaths node dacVisited fftVisited memo =
            case Map.lookup (node, dacVisited, fftVisited) memo of
                Just res -> (res, memo)
                Nothing ->
                    let
                        dacVisited' = dacVisited || node == "dac"
                        fftVisited' = fftVisited || node == "fft"
                    in 
                        if node == "out" 
                        then
                            let 
                                res = if dacVisited' && fftVisited' then 1 else 0
                                memo' = Map.insert (node, dacVisited', fftVisited') res memo
                            in (res, memo')
                        else
                            let
                                successors = Map.findWithDefault [] node graph
                                (cnt, memo') = foldl' (\(acc, accMemo) cur ->
                                        let 
                                            (res, curMemo) = countPaths cur dacVisited' fftVisited' accMemo
                                        in (acc + res, curMemo)
                                    )
                                    (0, memo)
                                    successors
                                memo'' = Map.insert (node, dacVisited, fftVisited) cnt memo'
                            in (cnt, memo'')

main :: IO()
main = do
    input <- lines <$> readFile "11.in"
    let 
        nodes = parseLine <$> input
        graph = Map.fromList nodes
    print $ dfs graph