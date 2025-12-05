maxJoltage :: String -> Integer
maxJoltage digits = read $ pick 12 digits
  where
    pick 0 _ = ""
    pick n xs =
      let candidate = take (length xs - n + 1) xs
          best = maximum candidate
          rem = drop (1 + length (takeWhile (/= best) candidate)) xs
       in best : pick (n - 1) rem

totalJoltage :: [String] -> Integer
totalJoltage = sum . map maxJoltage

main :: IO ()
main = do
  input <- lines <$> readFile "3.in"
  print $ totalJoltage input
