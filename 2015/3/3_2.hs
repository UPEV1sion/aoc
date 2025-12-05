import qualified Data.Set as Set

convert :: String -> [(Int, Int)]
convert = map charToMove
  where
    charToMove c = case c of
      'v' -> (1, 0)
      '^' -> (-1, 0)
      '>' -> (0, 1)
      '<' -> (0, -1)
      _ -> error "Invalid input"

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (a, b) (c, d) = (a + c, b + d)

main :: IO ()
main = do
  directions <- convert <$> readFile "3.in"
  let indexed = zip [0..] directions
      santaPositions = Set.fromList $ scanl add (0, 0) [x | (i,x) <- indexed, even i]
      robotPositions = Set.fromList $ scanl add (0, 0) [x | (i,x) <- indexed, odd i]
  print $ Set.size $ Set.union santaPositions robotPositions
