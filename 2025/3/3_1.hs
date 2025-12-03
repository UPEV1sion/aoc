largestTwoDigitNum :: String -> Int
largestTwoDigitNum cs = maximum [read [a, b] | (i, a) <- zip [0 ..] cs, (j, b) <- zip [0 ..] cs, j > i]

main :: IO ()
main = do
  banks <- lines <$> readFile "3.in"
  print $ sum $ map largestTwoDigitNum banks
