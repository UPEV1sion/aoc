import Data.List (sort)
import Data.List.Split (splitOn)

calcRectCuboid :: String -> Int
calcRectCuboid xs =
  let [l, w, h] = map read (splitOn "x" xs) :: [Int]
      [a, b, _] = sort [l, w, h]
      wrap = 2 * (a + b)
      bow = l * w * h
   in wrap + bow

main :: IO ()
main = do
  dimensions <- lines <$> readFile "2.in"
  print $ sum $ calcRectCuboid <$> dimensions