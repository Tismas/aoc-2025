import Data.List (elemIndex, find, sortBy)
import Data.Map (Map, findWithDefault, fromList, insert, size, union)
import Data.Maybe (isJust)
import Data.Ord (Down (Down), comparing)
import GHC.Base (maxInt)

data Position = Position
  { x :: Int,
    y :: Int,
    z :: Int
  }
  deriving (Show, Eq, Ord)

data Pair = Pair {a :: Position, b :: Position, dist :: Int} deriving (Show)

split :: Char -> String -> [String]
split _ "" = []
split separator input = x : split separator (drop 1 rest)
  where
    (x, rest) = break (== separator) input

readPosition :: String -> Position
readPosition line = Position {x, y, z}
  where
    splitted = split ',' line
    x = read (head splitted) :: Int
    y = read (splitted !! 1) :: Int
    z = read (last splitted) :: Int

distanceSq :: Position -> Position -> Int
distanceSq a b = dx * dx + dy * dy + dz * dz
  where
    dx = x a - x b
    dy = y a - y b
    dz = z a - z b

getAllPairs :: [Position] -> [Pair]
getAllPairs [] = []
getAllPairs (first : rest) = [Pair {a = first, b = x, dist = distanceSq first x} | x <- rest] ++ getAllPairs rest

getClosestPairs :: [Pair] -> [Pair]
getClosestPairs = sortBy (\a b -> compare (dist a) (dist b))

remove :: (Eq a) => [a] -> a -> [a]
remove list toRemove = case maybeI of
  Nothing -> list
  Just i -> take i list ++ drop (i + 1) list
  where
    maybeI = elemIndex toRemove list

replace :: (Eq a) => [a] -> a -> a -> [a]
replace list original updated = case maybeI of
  Nothing -> list
  Just i -> take i list ++ updated : drop (i + 1) list
  where
    maybeI = elemIndex original list

connect :: [Pair] -> Int -> [Map Position Bool] -> Pair -> Int -> ([Map Position Bool], Pair)
connect [] _ groups lastConnection _ = (groups, lastConnection)
connect _ 0 groups lastConnection _ = (groups, lastConnection)
connect (first : rest) toConnect groups lastConnection totalNodes = connect rest (toConnect - 1) newGroups newLastConnection totalNodes
  where
    maybeGroupA = find (findWithDefault False (a first)) groups
    maybeGroupB = find (findWithDefault False (b first)) groups
    alreadyConnected = maybeGroupA == maybeGroupB && isJust maybeGroupA
    newLastConnection = if size (head newGroups) == totalNodes && size (head groups) /= totalNodes then first else lastConnection
    newGroups = case (maybeGroupA, maybeGroupB) of
      (Just groupA, Just groupB) -> if alreadyConnected then groups else replace (remove groups groupA) groupB (groupA `union` groupB)
      (Just groupA, Nothing) -> replace groups groupA (insert (b first) True groupA)
      (Nothing, Just groupB) -> replace groups groupB (insert (a first) True groupB)
      (Nothing, Nothing) -> fromList [(a first, True), (b first, True)] : groups

main :: IO ()
main = do
  let test = False
  input <- if test then readFile "./src/day8/test.txt" else readFile "./src/day8/input.txt"
  let connectionCount = if test then 10 else 1000

  let positions = map readPosition (lines input)
  let totalNodes = length positions
  let sortedPairs = getClosestPairs (getAllPairs positions)
  let connected = connect sortedPairs connectionCount [] (head sortedPairs) totalNodes
  let part1 = product (take 3 (sortBy (comparing Data.Ord.Down) (map length . fst $ connected)))
  print part1

  let lastConnection = snd (connect sortedPairs maxInt [] (head sortedPairs) totalNodes)
  let part2 = x (a lastConnection) * x (b lastConnection)
  print part2