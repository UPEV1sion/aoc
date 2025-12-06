import Data.List ( transpose )
import Data.List.Split (splitWhen)
import Data.Char (isDigit)

evalBlock :: [String] -> String -> Int
evalBlock cols op =
    let nums = map read cols
    in case op of
        "+" -> sum nums
        "*" -> product nums

isBlankCol :: String -> Bool
isBlankCol = all (== ' ')

splitBlocks :: [String] -> [[String]]
splitBlocks = filter (not . null) . splitWhen isBlankCol

main :: IO()
main = do
    input <- lines <$> readFile "6.in"
    let vertNums = transpose $ init input
        ops = words $ last input
        blocks = splitBlocks vertNums
    print $ sum $ zipWith evalBlock blocks ops
