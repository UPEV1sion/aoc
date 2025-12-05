import Data.List.Split (splitOn)

calcRectCuboid :: String -> Int
calcRectCuboid xs =
  let [l, w, h] = read <$> splitOn "x" xs
      sides = [l * w, w * h, h * l]
   in 2 * sum sides + minimum sides

main :: IO ()
main = do
  dimensions <- lines <$> readFile "2.in"
  print $ sum $ calcRectCuboid <$> dimensions