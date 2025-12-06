import Data.List ( transpose )

evalCol :: [String] -> Int
evalCol col = 
    let (ops, [op]) = splitAt 4 col
        nums = read <$> ops
    in case op of
        "+" -> sum nums
        "*" -> product nums

main :: IO()
main = do
    input <- lines <$> readFile "6.in"
    let cols = transpose $ words <$> input
    print $ sum $ evalCol <$> cols
