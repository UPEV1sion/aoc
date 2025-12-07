import qualified Data.Set as Set
import Data.List (isInfixOf)

forbiddenSeqs :: [String]
forbiddenSeqs = ["ab", "cd", "pq", "xy"]

containsForbidden :: String -> Bool
containsForbidden str = any (`isInfixOf` str) forbiddenSeqs

vowels :: Set.Set Char
vowels = Set.fromList "aeiou"

containsThreeVowels :: String -> Bool
containsThreeVowels str = (length $ filter (`Set.member` vowels) str) >= 3

containsRepeated (x:y:xs) 
    | x == y    = True
    | otherwise = containsRepeated (y:xs)
containsRepeated _ = False

main :: IO()
main = do
    input <- lines <$> readFile "5.in"
    print $ length $ filter (\str ->  (containsThreeVowels str) && (not $ containsForbidden str) && (containsRepeated str)) input
