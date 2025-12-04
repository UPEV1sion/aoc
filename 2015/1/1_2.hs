convert :: String -> [Int]
convert [] = []
convert (x : xs)
  | x == '(' = 1 : convert xs
  | x == ')' = -1 : convert xs
  | otherwise = error "Invalid input"

main :: IO ()
main = do
  braces <- readFile "1.in"
  let positions = scanl (+) 0 (convert braces)
  print $ length $ takeWhile (/= -1) positions