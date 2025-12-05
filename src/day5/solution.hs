import Data.List (sort)

data Range = Range
  { low :: Int,
    high :: Int
  }
  deriving (Eq, Ord, Show)

isInRange :: Int -> Range -> Bool
isInRange value range = value >= low range && value <= high range

consolidateRanges :: [Range] -> [Range]
consolidateRanges [] = []
consolidateRanges [x] = [x]
consolidateRanges (first : second : rest)
  | low second <= high first = consolidateRanges (Range (low first) (max (high first) (high second)) : rest)
  | otherwise = first : consolidateRanges (second : rest)

rangeSize :: Range -> Int
rangeSize range = high range - low range + 1

split :: Char -> String -> [String]
split _ "" = []
split separator input = x : split separator (drop 1 rest)
  where
    (x, rest) = break (== separator) input

createRange :: String -> Range
createRange range = Range {low = read start, high = read end}
  where
    arr = split '-' range
    start = head arr
    end = arr !! 1

main :: IO ()
main = do
  let test = False
  input <- if test then readFile "./src/day5/test.txt" else readFile "./src/day5/input.txt"
  let inputLines = lines input

  let (rangesRaw, idsRaw) = break (== "") inputLines
  let ids = map read (drop 1 idsRaw)
  let ranges = consolidateRanges (sort (map createRange rangesRaw))

  let part1 = length [x | x <- ids, any (isInRange x) ranges]
  print part1

  let part2 = sum (map rangeSize ranges)
  print part2