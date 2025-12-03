replaceDigit :: Char -> [Char] -> Int -> [Char]
replaceDigit _ [] _ = []
replaceDigit digit (first : rest) remaining
  | toBeErased > remaining = first : replaceDigit digit rest remaining
  | digit > first = digit : replicate (length rest) '0'
  | otherwise = first : replaceDigit digit rest remaining
  where
    toBeErased = length rest

getLargestNumber :: Int -> [Char] -> String -> [Char]
getLargestNumber digitsAmount [] line = getLargestNumber digitsAmount (replicate digitsAmount '0') line
getLargestNumber _ digits "" = digits
getLargestNumber digitsAmount digits (first : rest) = getLargestNumber digitsAmount (replaceDigit first digits (length rest)) rest

main :: IO ()
main = do
  let test = False
  input <- if test then readFile "./src/day3/test.txt" else readFile "./src/day3/input.txt"

  let inputLines = lines input
  let part1 = sum (map (read . getLargestNumber 2 []) inputLines) :: Integer
  print part1

  let part2 = sum (map (read . getLargestNumber 12 []) inputLines) :: Integer
  print part2
