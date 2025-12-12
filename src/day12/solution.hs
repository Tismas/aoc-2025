split :: Char -> String -> [String]
split _ "" = []
split separator input = x : split separator (drop 1 rest)
  where
    (x, rest) = break (== separator) input

canFit :: String -> Bool
canFit line = totalSpace >= neededSpace
  where
    (size : presentCounts) = words line
    (width : height : _) = map read (split 'x' (init size)) :: [Int]
    totalSpace = width * height
    neededSpace = sum (map read presentCounts) * 9

main :: IO ()
main = do
  let test = False
  input <- if test then readFile "./src/day12/test.txt" else readFile "./src/day12/input.txt"

  let inputLines = lines input
  let part1 = sum (map ((\b -> if b then 1 else 0) . canFit) inputLines)
  print part1

  let part2 = 0
  print part2
