import Control.Monad.State
import Data.Map (Map, empty, findWithDefault, fromList, unionWith)
import Data.Map qualified as M

type Cache = M.Map (String, String) Int

type Connections = Map String [String]

parseLine :: String -> Connections
parseLine line = fromList [(from, to)]
  where
    (fromRaw : to) = words line
    from = init fromRaw

findAllPathsMemo :: Connections -> String -> String -> State Cache Int
findAllPathsMemo connections from to = do
  cache <- get
  case M.lookup (from, to) cache of
    Just v -> return v
    Nothing -> do
      let connectionsFrom = findWithDefault [] from connections
      res <- case () of
        _
          | null connectionsFrom -> return 0
          | to `elem` connectionsFrom -> return 1
          | otherwise -> sum <$> mapM (\n -> findAllPathsMemo connections n to) connectionsFrom

      modify (M.insert (from, to) res)
      return res

runFind :: Connections -> String -> String -> Int
runFind connections from to = evalState (findAllPathsMemo connections from to) M.empty

main :: IO ()
main = do
  let test = False
  input <- if test then readFile "./src/day11/test.txt" else readFile "./src/day11/input.txt"

  let inputLines = lines input
  let connections = foldl (unionWith (++)) Data.Map.empty (map parseLine inputLines)
  let part1 = runFind connections "you" "out"
  print part1

  let dac_out = runFind connections "dac" "out"
  let fft_out = runFind connections "fft" "out"

  let svr_fft = runFind connections "svr" "fft"
  let svr_dac = runFind connections "svr" "dac"

  let fft_dac = runFind connections "fft" "dac"
  let dac_fft = runFind connections "dac" "fft"

  print ((svr_dac * dac_fft * fft_out) + (svr_fft * fft_dac * dac_out))
