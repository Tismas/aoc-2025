split :: Char -> String -> [String]
split _ "" = []
split separator input = x : split separator (drop 1 rest)
  where
    (x, rest) = break (== separator) input

createRange :: String -> [Int]
createRange range = [read start .. read end]
  where
    arr = split '-' range
    start = head arr
    end = arr !! 1

isRepeated :: Int -> Int -> Bool
isRepeated times itemId
  | len `rem` times /= 0 = False
  | otherwise = idStr == concat (replicate times firstPart)
  where
    idStr = show itemId
    len = length idStr
    partLen = len `div` times
    firstPart = take partLen idStr

divisors :: Int -> [Int]
divisors n = [x | x <- [2 .. n], n `rem` x == 0]

isInvalidId :: Int -> Bool
isInvalidId itemId = or [isRepeated x itemId | x <- divisors . length . show $ itemId]

main :: IO ()
main = do
  let test = False
  input <- if test then readFile "./src/day2/test.txt" else readFile "./src/day2/input.txt"
  let ranges = map createRange (split ',' input)

  let part1 = sum (concatMap (filter (isRepeated 2)) ranges)
  print part1

  let part2 = sum (concatMap (filter isInvalidId) ranges)
  print part2
