import qualified Data.Map as Map

parseLine :: String -> (String, [String])
parseLine s = 
    let
        nodes = words s
        parent = head nodes
        successors = tail nodes
    in (init parent, successors)

dfs :: Map.Map String [String] -> Int
dfs graph = countPaths "you"
    where
        countPaths node 
            | node == "out" = 1
            | otherwise = 
                let
                    successors = Map.findWithDefault [] node graph
                in sum [countPaths n | n <- successors]

main :: IO()
main = do
    input <- lines <$> readFile "11.in"
    let 
        nodes = parseLine <$> input
        graph = Map.fromList nodes
    print $ dfs graph