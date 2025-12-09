import Data.List.Split (splitOn)

parsePoint :: String -> (Int, Int)
parsePoint str =
  case map read (splitOn "," str) of
    [x, y] -> (x, y)
    _ -> error "Invalid point format"

area :: (Int, Int) -> (Int, Int) -> Int
area (x1, y1) (x2, y2) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

buildAreas :: [(Int, Int)] -> [Int]
buildAreas points = [area t1 t2 | (i, t1) <- zip [0 ..] points, t2 <- drop (i + 1) points]

main :: IO ()
main = do
  ls <- lines <$> readFile "9.in"
  let points = parsePoint <$> ls

  print $ maximum $ buildAreas points