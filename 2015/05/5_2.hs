import qualified Data.Set as Set
import Text.Regex.PCRE
import Data.List (isInfixOf)

containsRepeatedChar :: String -> Bool
containsRepeatedChar (a:x:b:xs) = (a == b) || containsRepeatedChar (x:b:xs)
containsRepeatedChar [_, _] = False
containsRepeatedChar [_] = False
containsRepeatedChar [] = False

containsRepeatedTuplet :: String -> Bool
containsRepeatedTuplet s = s =~ "(\\w{2}).*\\1"

main :: IO()
main = do
    input <- lines <$> readFile "5.in"
    print $ length $ filter (\str ->  containsRepeatedTuplet str && containsRepeatedChar str) input
