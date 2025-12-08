import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.List (sortOn, sort)
import Data.Ord (Down(..))

newtype Point = Point [Int] deriving (Show, Eq, Ord)

data Edge = Edge {p1 :: Point, p2 :: Point, w :: Double}

type Parent = Map.Map Point Point
type Size   = Map.Map Point Int

cost :: Point -> Point -> Double
cost (Point xs) (Point ys) = sqrt . sum $ zipWith (\a b -> fromIntegral (b - a)^2) xs ys

parsePoint :: String -> Point
parsePoint str = Point (read <$> splitOn "," str)

allEdges :: [Point] -> [Edge]
allEdges points = [ Edge p1 p2 (cost p1 p2)
                  | (i,p1) <- zip [0..] points
                  , p2 <- drop (i+1) points
                  ]

find :: Parent -> Point -> (Point, Parent)
find parent p =
    case Map.lookup p parent of
        Nothing -> (p, parent)
        Just x  ->
            if x == p
            then (p, parent)
            else
                let (rootX, parent') = find parent x
                in (rootX, Map.insert p x parent')

union :: (Parent, Size) -> Point -> Point -> (Parent, Size)
union (parent, size) x y =
    let
        (rootX, parent1) = find parent x
        (rootY, parent2) = find parent1 y
    in
        if rootX == rootY
        then (parent, size)
        else
            let
                sizeX = Map.findWithDefault 1 rootX size
                sizeY = Map.findWithDefault 1 rootY size
                (newRoot, oldRoot) = if sizeX >= sizeY then (rootX, rootY) else (rootY, rootX)
                newSize   = Map.insert newRoot (sizeX + sizeY) (Map.delete oldRoot size)
                newParent = Map.insert oldRoot newRoot parent2
            in
                (newParent, newSize)

kruskal :: [Point] -> [Edge] -> (Parent, Size)
kruskal points edges =
    let
        sortedEdges = take 1000 $ sortOn w edges
        initParent  = Map.fromList [(p,p) | p <- points]
        initCost    = Map.fromList [(p,1) | p <- points]
    in foldl (\ds e -> union ds (p1 e) (p2 e)) (initParent,initCost) sortedEdges

main :: IO ()
main = do
    input <- lines <$> readFile "8.in"
    let
        points = parsePoint <$> input
        edges  = allEdges points
        (_, size) = kruskal points edges
        top3 = take 3 $ reverse $ sort (Map.elems size)
    print $ product top3