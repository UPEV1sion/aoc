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

isRepeating :: Integer -> Bool
isRepeating n =
  any
    ( \len ->
        let (block1, block2) = splitAt len s
         in block1 == block2 && length block1 * 2 == length s
    )
    [1 .. (length s `div` 2)]
  where
    s = show n

main :: IO ()
main = do
  ranges <- split ',' <$> readFile "2.in"
  let startStop = map getRange ranges
  let repeatingNumbers = [n | (a, b) <- startStop, n <- [a .. b], isRepeating (toInteger n)]
  print $ sum repeatingNumbers
