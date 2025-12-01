transform :: String -> Int
transform ('L' : cs) = - read cs
transform ('R' : cs) = read cs
transform _ = error "Unknown input"

applyRot :: Int -> Int -> Int
applyRot pos rot = (pos + rot) `mod` 100

main :: IO ()
main = do
  xs <- map transform . lines <$> readFile "1.in"
  let running = tail (scanl applyRot 50 xs)
  print $ length (filter (== 0) running)