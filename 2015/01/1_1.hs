getFloor :: Int -> String -> Int
getFloor acc [] = 0
getFloor acc (x : xs)
  | x == '(' = getFloor acc xs + 1
  | x == ')' = getFloor acc xs - 1
  | otherwise = error "Invalid Input"

main :: IO ()
main = do
  braces <- readFile "1.in"
  print $ getFloor 0 braces