import Data.Char

toInt :: String -> Int
toInt [] = 0
toInt (x:xs) = (ord x - ord '0') * 10 ^ length xs + toInt xs 

main :: IO()
main = print $ toInt "123"