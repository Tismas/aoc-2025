import Control.Monad.State
import Data.Function (on)
import Data.List (minimumBy, subsequences)
import Data.Map qualified as M

data Machine = Machine
  { lights :: String,
    buttons :: [[Int]],
    joltage :: [Int]
  }
  deriving (Show)

type Cache = M.Map [Int] Int

split :: Char -> String -> [String]
split _ "" = []
split separator input = x : split separator (drop 1 rest)
  where
    (x, rest) = break (== separator) input

dropFirstAndLast :: [a] -> [a]
dropFirstAndLast arr = take (length arr - 2) (drop 1 arr)

readNumList :: String -> [Int]
readNumList line = map read (split ',' (dropFirstAndLast line))

parseLine :: String -> Machine
parseLine line = Machine {lights = lights, buttons = buttons, joltage = joltage}
  where
    sections = words line

    rawLights = head sections
    lights = dropFirstAndLast rawLights

    rawButtons = dropFirstAndLast sections
    buttons = map readNumList rawButtons

    rawJoltage = last sections
    joltage = readNumList rawJoltage

toggleLight :: Char -> Char
toggleLight c = if c == '.' then '#' else '.'

toggleLights :: String -> [Int] -> String
toggleLights lights indexes = [if i `elem` indexes then toggleLight value else value | (i, value) <- zip [0 ..] lights]

solveLights :: String -> [[Int]] -> [[[Int]]]
solveLights targetLights buttons = solutions
  where
    options = subsequences buttons
    lights = replicate (length targetLights) '.'
    toggled = [foldl toggleLights lights pressedButtons | pressedButtons <- options]
    solutions = [options !! index | (value, index) <- zip toggled [0 .. length toggled - 1], value == targetLights]

getNewTarget :: [Int] -> [[Int]] -> [Int]
getNewTarget target presses = [(target !! i - length (filter (\v -> i `elem` v) presses)) `div` 2 | i <- [0 .. length target - 1]]

solveButtons :: [[Int]] -> [Int] -> State Cache Int
solveButtons buttons target = do
  cache <- get
  case M.lookup target cache of
    Just v -> return v
    Nothing -> do
      res <-
        if done
          then
            return 0
          else
            if invalid
              then
                return 100_000
              else do
                values <- mapM totalValue solutions
                return (minimum values)
      modify (M.insert target res)
      return res
  where
    done = all (== 0) target
    oddToLights = map (\x -> if x `mod` 2 == 1 then '#' else '.') target
    solutions = solveLights oddToLights buttons
    invalid = any (< 0) target || null solutions

    totalValue presses = do
      sub <- solveButtons buttons (getNewTarget target presses)
      return (length presses + 2 * sub)

runSolveButtons :: [[Int]] -> [Int] -> Int
runSolveButtons buttons target = evalState (solveButtons buttons target) M.empty

main :: IO ()
main = do
  let test = False
  input <- if test then readFile "./src/day10/test.txt" else readFile "./src/day10/input.txt"

  let inputLines = lines input
  let machines = map parseLine inputLines
  let part1Solutions = [solveLights (lights machine) (buttons machine) | machine <- machines]
  let part1 = map (minimumBy (compare `on` length)) part1Solutions
  print (sum (map length part1))

  let part2 = [runSolveButtons (buttons machine) (joltage machine) | machine <- machines]
  print (sum part2)