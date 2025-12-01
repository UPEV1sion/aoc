import Data.List (mapAccumL)

transform :: String -> Int
transform ('L' : cs) = - read cs
transform ('R' : cs) = read cs
transform _ = error "Unknown input"

countCross :: Int -> Int -> Int
countCross st del
  | del == 0 = 0
  | del > 0 = (st + del) `div` 100
  | otherwise =
    let full = (- del) `div` 100
        remind = del `rem` 100
        final = if st > 0 && st + remind <= 0 then 1 else 0
     in full + final

step :: Int -> Int -> (Int, Int)
step pos rot =
  let pos' = (pos + rot) `mod` 100
      crosses = countCross pos rot
   in (pos', crosses)

wrapCount :: Int -> [Int] -> Int
wrapCount start xs =
  let (_, crosses) = mapAccumL step start xs
   in sum crosses

main :: IO ()
main = do
  xs <- map transform . lines <$> readFile "1.in"
  print $ wrapCount 50 xs