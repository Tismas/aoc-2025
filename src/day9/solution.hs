import Data.Foldable (maximumBy)
import Data.Function (on)

data Position = Position
  { x :: Int,
    y :: Int
  }
  deriving (Show, Eq, Ord)

type Line = (Position, Position)

type Rect = (Position, Position)

split :: Char -> String -> [String]
split _ "" = []
split separator input = x : split separator (drop 1 rest)
  where
    (x, rest) = break (== separator) input

getArea :: Rect -> Int
getArea (first, second) = (abs (x first - x second) + 1) * (abs (y first - y second) + 1)

getLargestArea :: [Position] -> Int
getLargestArea [] = 0
getLargestArea (first : rest) = maximum (getLargestArea rest : [getArea (first, second) | second <- rest])

getLines :: [Position] -> [Line]
getLines [] = []
getLines [_] = []
getLines (first : second : rest) = (first, second) : getLines (second : rest)

sortPair :: Int -> Int -> (Int, Int)
sortPair a b = if a < b then (a, b) else (b, a)

isBetween :: Int -> Int -> Int -> Bool
isBetween value a b = value > smaller && value < larger
  where
    (smaller, larger) = sortPair a b

parseLine :: String -> Position
parseLine line = Position {x, y}
  where
    splitted = split ',' line
    x = read (head splitted)
    y = read (last splitted)

isOverlapping :: Int -> Int -> Int -> Int -> Bool
isOverlapping x1 x2 x3 x4 = mostRightStart < mostLeftEnd
  where
    (smx, lgx) = sortPair x1 x2
    (smx2, lgx2) = sortPair x3 x4
    mostLeftEnd = min lgx lgx2
    mostRightStart = max smx smx2

isIntersecting :: Rect -> Line -> Bool
isIntersecting (ra, rb) (la, lb) = overlap
  where
    isVertical = x la == x lb
    overlap = if isVertical then verticalOverlap else horizontalOverlap
    verticalOverlap = isBetween (x la) (x ra) (x rb) && isOverlapping (y la) (y lb) (y ra) (y rb)
    horizontalOverlap = isBetween (y la) (y ra) (y rb) && isOverlapping (x la) (x lb) (x ra) (x rb)

getRectIfInside :: [Line] -> Rect -> Maybe Rect
getRectIfInside greenLines rect = if isOutside then Nothing else Just rect
  where
    isOutside = any (isIntersecting rect) greenLines

getMaybeArea :: Maybe Rect -> Int
getMaybeArea maybeRect = case maybeRect of
  Just rect -> getArea rect
  _ -> 0

getLargestRectInside :: [Line] -> [Position] -> Maybe Rect
getLargestRectInside _ [] = Nothing
getLargestRectInside greenLines (first : rest) = maximumBy (compare `on` getMaybeArea) (getLargestRectInside greenLines rest : [getRectIfInside greenLines (first, second) | second <- rest])

main :: IO ()
main = do
  let test = False
  input <- if test then readFile "./src/day9/test.txt" else readFile "./src/day9/input.txt"

  let inputLines = lines input
  let points = map parseLine inputLines
  let part1 = getLargestArea points
  print part1

  let greenLines = getLines points ++ [(last points, head points)]
  let part2Rect = getLargestRectInside greenLines points
  print (getMaybeArea part2Rect)