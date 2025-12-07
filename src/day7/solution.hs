import Data.List (elemIndex)
import Data.Map (Map, delete, findWithDefault, fromList, insertWith)
import Data.Maybe (fromMaybe)

indexOf :: Char -> String -> Int
indexOf c str = fromMaybe (-1) (elemIndex c str)

processBeams :: String -> Map Int Int -> Int -> Bool -> (Map Int Int, Int)
processBeams line beams splits quantum = if splitterIndex == -1 then (newBeams, newSplits) else processBeams newLine newBeams newSplits quantum
  where
    splitterIndex = indexOf '^' line
    hittingBeams = findWithDefault (-1) splitterIndex beams
    insertedLeft = insertWith (+) (splitterIndex - 1) hittingBeams beams
    insertedBoth = insertWith (+) (splitterIndex + 1) hittingBeams insertedLeft
    newBeams = if hittingBeams /= -1 then delete splitterIndex insertedBoth else beams
    newSplits = if hittingBeams /= -1 then splits + (if quantum then hittingBeams else 1) else splits
    newLine = replicate (splitterIndex + 1) '.' ++ drop (splitterIndex + 1) line

simulate :: [String] -> Map Int Int -> Int -> Bool -> Int
simulate [] _ splits _ = splits
simulate (line : rest) beams splits quantum = simulate rest newBeams newSplits quantum
  where
    (newBeams, newSplits) = processBeams line beams splits quantum

main :: IO ()
main = do
  let test = False
  input <- if test then readFile "./src/day7/test.txt" else readFile "./src/day7/input.txt"

  let (startLine : rest) = lines input
  let startingPos = fromList [(indexOf 'S' startLine, 1)]
  let part1 = simulate rest startingPos 0 False
  print part1

  let part2 = simulate rest startingPos 0 True + 1
  print part2
