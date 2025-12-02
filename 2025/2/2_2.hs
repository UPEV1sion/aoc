split :: Char -> String -> [String]
split _ "" = []
split delim cs =
  let (chunk, rest) = break (== delim) cs
   in chunk : case rest of
        [] -> []
        (_ : xs) -> split delim xs

getRange :: String -> (Int, Int)
getRange cs = (read start, read $ tail stop)
  where
    (start, stop) = break (== '-') cs

isRepeating :: Show a => a -> Bool
isRepeating n = any repeats [1 .. (length s `div` 2)]
  where
    s = show n
    repeats len =
      let block = take len s
       in block /= "" && concat (replicate (length s `div` len) block) == s

main :: IO ()
main = do
  ranges <- split ',' <$> readFile "2.in"
  let startStop = map getRange ranges
  let repeatingNumbers = [n | (a, b) <- startStop, n <- [a .. b], isRepeating (toInteger n)]
  print $ sum repeatingNumbers
