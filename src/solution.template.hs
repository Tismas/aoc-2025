main :: IO ()
main = do
  let test = True
  input <- if test then readFile "./src/dayx/test.txt" else readFile "./src/dayx/input.txt"

  let inputLines = lines input
  let part1 = 0
  print part1

  let part2 = 0
  print part2
