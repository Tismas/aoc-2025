import Data.Char (isDigit)
import Data.List (transpose)
import Distribution.Utils.String (trim)

split :: String -> [String] -> [[String]]
split _ [] = []
split separator input = x : split separator (drop 1 rest)
  where
    (x, rest) = break (== separator) input

calculate :: [String] -> Int
calculate column = foldl fn initialValue numbers
  where
    isMult = trim (last column) == "*"
    fn = if isMult then (*) else (+)
    initialValue = if isMult then 1 else 0
    numbers = map (read . trim) (init column)

weirdCalculate :: [String] -> Int
weirdCalculate column = foldl fn initialValue numbers
  where
    isMult = trim (last column) == "*"
    fn = if isMult then (*) else (+)
    initialValue = if isMult then 1 else 0
    numbers = [(read . trim) (filter isDigit x) | x <- transpose column]

main :: IO ()
main = do
  let test = False
  input <- if test then readFile "./src/day6/test.txt" else readFile "./src/day6/input.txt"

  let inputLines = lines input
  let emptyCol = replicate (length inputLines) ' '
  let columns = map transpose (split emptyCol (transpose inputLines))

  let part1 = sum (map calculate columns)
  print part1

  let part2 = sum (map weirdCalculate columns)
  print part2
