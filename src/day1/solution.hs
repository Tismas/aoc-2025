decodeLine :: String -> Integer
decodeLine ('L' : steps) = -(read steps)
decodeLine ('R' : steps) = read steps
decodeLine _ = 0

isZero :: Integer -> Integer
isZero 0 = 1
isZero _ = 0

getStopsAt0 :: [String] -> Integer -> Integer -> Integer
getStopsAt0 [] _ zeros = zeros
getStopsAt0 (first : rest) currentValue zeros = getStopsAt0 rest nextValue nextZeros
  where
    nextValue = (currentValue + decodeLine first) `mod` 100
    nextZeros = zeros + isZero nextValue

getPassesAt0 :: Integer -> Integer -> Integer
getPassesAt0 currentValue nextValue
  | nextValue == 0 = 0
  | nextValue `mod` 100 == 0 = overflow - 1 + signDiff
  | otherwise = signDiff + overflow
  where
    signDiff = if (currentValue < 0) /= (nextValue < 0) && currentValue /= 0 then 1 else 0
    overflow = floor ((fromInteger . abs $ nextValue) / 100 :: Double)

getPassesAndStopsAt0 :: [String] -> Integer -> Integer -> Integer
getPassesAndStopsAt0 [] _ zeros = zeros
getPassesAndStopsAt0 (first : rest) currentValue zeros = getPassesAndStopsAt0 rest nextValue nextZeros
  where
    nextValueRaw = currentValue + decodeLine first
    nextValue = nextValueRaw `mod` 100
    nextZeros = zeros + isZero nextValue + getPassesAt0 currentValue nextValueRaw

main :: IO ()
main = do
  let test = True
  input <- if test then readFile "./src/day1/test.txt" else readFile "./src/day1/input.txt"

  let inputLines = lines input
  let startingValue = 50
  let part1 = getStopsAt0 inputLines startingValue 0
  print part1

  let part2 = getPassesAndStopsAt0 inputLines startingValue 0
  print part2
