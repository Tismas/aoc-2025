getSurroundingLocations :: Int -> Int -> [(Int, Int)]
getSurroundingLocations x y =
  [ (x + dx, y + dy)
    | dx <- [-1, 0, 1],
      dy <- [-1, 0, 1],
      (dx, dy) /= (0, 0)
  ]

getSurroundingPaper :: [[Char]] -> Int -> Int -> Int
getSurroundingPaper grid x y = sum [1 | (dx, dy) <- getSurroundingLocations x y, dx >= 0 && dy >= 0 && dx < width && dy < height && grid !! dy !! dx == '@']
  where
    width = length . head $ grid
    height = length grid

getRemovablePaper :: [[Char]] -> [(Int, Int)]
getRemovablePaper grid = [(x, y) | x <- [0 .. length grid - 1], y <- [0 .. length (head grid) - 1], grid !! y !! x == '@', getSurroundingPaper grid x y < 4]

removePaper :: [[Char]] -> Int -> Int
removePaper grid removed
  | null toRemove = removed
  | otherwise = removePaper updatedGrid (removed + length toRemove)
  where
    toRemove = getRemovablePaper grid
    updatedGrid =
      [ [ if (x, y) `elem` toRemove then '.' else node
          | let row = grid !! y,
            x <- [0 .. length (grid !! y) - 1],
            let node = row !! x
        ]
        | y <- [0 .. length grid - 1]
      ]

main :: IO ()
main = do
  let test = False
  input <- if test then readFile "./src/day4/test.txt" else readFile "./src/day4/input.txt"

  let grid = lines input
  let part1 = length . getRemovablePaper $ grid
  print part1

  let part2 = removePaper grid 0
  print part2
