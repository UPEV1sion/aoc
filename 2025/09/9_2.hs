import Data.List.Split (splitOn)
import Data.List (foldl', tails)

type Point = (Int, Int)
type Edge = (Point, Point)

parsePoint :: String -> (Int, Int)
parsePoint str =
  case map read (splitOn "," str) of
    [x, y] -> (x, y)
    _ -> error "Invalid point format"

area :: (Int, Int) -> (Int, Int) -> Int
area (x1, y1) (x2, y2) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

sortPair :: (Int, Int) -> (Int, Int)
sortPair (p1, p2) = if p1 < p2 then (p1, p2) else (p2, p1)

intersects :: Point -> Point -> Edge -> Bool
intersects (minX, minY) (maxX, maxY) ((ex1, ey1), (ex2, ey2)) =
    let
        (edgeMinX, edgeMaxX) = sortPair (ex1, ex2)
        (edgeMinY, edgeMaxY) = sortPair (ey1, ey2)
    in minX < edgeMaxX && maxX > edgeMinX && minY < edgeMaxY &&  maxY > edgeMinY

manhattan :: Point -> Point -> Int
manhattan (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

intersectsAny :: Point -> Point -> [Edge] -> Bool
intersectsAny p1 p2 = any (intersects p1 p2)

findLargestArea :: [Point] -> Int
findLargestArea points = maxArea
    where
        edges = zip points (drop 1 points ++ [head points])
        allPairs = [(p1, p2) | (p1:rest) <- tails points, p2 <- (p1:rest) ]
        maxArea = foldl' (\acc (p1, p2) ->
            let
                (minX, maxX) = sortPair (fst p1, fst p2)
                (minY, maxY) = sortPair (snd p1, snd p2)
                manh = manhattan (minX, minY) (maxX, maxY)
            in (if (manh * manh <= acc) || intersectsAny (minX, minY) (maxX, maxY) edges
                then acc
                else max acc (area p1 p2))
            ) 0 allPairs

main :: IO ()
main = do
  ls <- lines <$> readFile "9.in"
  let points = parsePoint <$> ls
  print $ findLargestArea points