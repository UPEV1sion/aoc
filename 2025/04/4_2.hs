countNeighbours :: Int -> Int -> Int -> Int -> [String] -> Int
countNeighbours r c nRows nCols grid = length 
    [() | dr <- [-1..1], dc <- [-1..1], (dr,dc) /= (0,0), 
    let 
        nr = r + dr
        nc = c + dc,
        nr >= 0, nc >= 0, nr < nRows, nc < nCols,
        (grid !! nr !! nc) == '@'] 

countAdjacents :: [String] -> Int
countAdjacents grid = 
    let 
        nRows = length grid
        nCols = length (head grid)
    in length [()
              | i <- [0..nRows - 1], j <- [0..nCols - 1]
              , grid !! i !! j == '@'
              , countNeighbours i j nRows nCols grid  < 4
              ]

step :: [String] -> [String]
step grid = 
    let
        nRows = length grid
        nCols = length (head grid)
    in
    [[ if cell == '@' && countNeighbours r c nRows nCols grid < 4 then '.' else cell
        | (c, cell) <- zip [0..] row]
    | (r, row) <- zip [0..] grid]

fixpoint :: Eq t => (t -> t) -> t -> t
fixpoint f x = 
    let x' = f x
    in if x' == x then x else fixpoint f x'

main :: IO()
main = do 
    grid <- lines <$> readFile "4.in"
    let finalGrid = fixpoint step grid
        removed = countChars '@' grid - countChars '@' finalGrid
    print removed
    where
        countChars c = sum . map (length . filter (== c))