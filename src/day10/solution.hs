import Z3.Monad
import Control.Monad (forM, forM_)
import Data.Maybe (catMaybes)

data Machine = Machine
  { lights :: String,
    buttons :: [[Int]],
    joltage :: [Int]
  }
  deriving (Show)

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

solveLights :: [String] -> String -> [[Int]] -> Int -> Int
solveLights lightsArr targetLights buttons presses
  | found = presses + 1
  | otherwise = solveLights toggled targetLights buttons (presses + 1)
  where
    toggled = [toggleLights lights button | button <- buttons, lights <- lightsArr]
    found = targetLights `elem` toggled

solveButtons :: [[Int]] -> [Int] -> IO Double
solveButtons buttons target = evalZ3 $ do
  let nButtons = length buttons
  vars <- mapM (\i -> mkStringSymbol ("x_" ++ show i) >>= mkIntVar) [0 .. nButtons - 1]

  zero <- mkInteger 0
  mapM_ (\v -> mkGe v zero >>= assert) vars

  mapM_ (\(dimIdx, tgtVal) -> do
      tgt <- mkInteger (fromIntegral tgtVal)
      terms <- mapM (\(j, v) -> do
          let btn = buttons !! j
          if dimIdx `elem` btn
            then return (Just v)
            else return Nothing
        ) (zip [0..] vars)
      let activeVars = catMaybes terms
      sumTerm <- if null activeVars then mkInteger 0 else mkAdd activeVars
      mkEq sumTerm tgt >>= assert
    ) (zip [0..] target)

  totalPresses <- mkAdd vars
  
  let loop currentMin = do
        result <- solverCheck
        case result of
          Sat -> do
            model <- solverGetModel
            sumVal <- evalInt model totalPresses
            case sumVal of
              Just s -> do
                sAst <- mkInteger s
                mkLt totalPresses sAst >>= assert
                loop (Just s)
              Nothing -> return currentMin
          _ -> return currentMin

  finalMin <- loop Nothing
  case finalMin of
    Just val -> return (fromIntegral val)
    Nothing -> error "No feasible button combination"

main :: IO ()
main = do
  let test = False
  input <- if test then readFile "./src/day10/test.txt" else readFile "./src/day10/input.txt"

  let inputLines = lines input
  let machines = map parseLine inputLines
  let part1 = [solveLights [replicate (length (lights machine)) '.'] (lights machine) (buttons machine) 0 | machine <- machines]
  print (sum part1)

  part2 <- mapM (\machine -> solveButtons (buttons machine) (joltage machine)) machines
  print (sum part2)