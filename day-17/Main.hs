import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs)
import Text.Parsec (many1)
import Text.Parsec.Char (char)
import Text.Parsec.String (Parser, parseFromFile)
import Prelude hiding (Left, Right)

shapes :: [Shape]
shapes =
  [ Set.fromList [(0, 0), (1, 0), (2, 0), (3, 0)],
    {-
    ####
    -}
    Set.fromList [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
    {-
    .#.
    ###
    .#.
    -}
    Set.fromList [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)],
    {-
    ..#
    ..#
    ###
    -}
    Set.fromList [(0, 0), (0, 1), (0, 2), (0, 3)],
    {-
    #
    #
    #
    #
    -}
    Set.fromList [(0, 0), (1, 0), (0, 1), (1, 1)]
    {-
    ##
    ##
    -}
  ]

width :: Int
width = 7

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let jets = cycle input
  -- print input
  let state = State [] jets (initializeShape state (head shapes) : tail (cycle shapes))
  -- print $ head $ _shapes state
  print $ topY (iterate step state !! 2022) + 1
  return ()

{-
  coordinates: (0, 0) is the bottom left corner

  algorithm:
  - if next step of shape intersects with map, settle and merge; generate next shape 3 spaces apart from the top of the map
  - otherwise, take a step of being pushed by jet and falling down

  one step:
  - being pushed by jet: take one jet direction, move all coords of shape in that direction if none is out of bounds
  - falling down: move all coords of shape down by one

  settle and merge:
  - add all coords of shape to map

  generate next shape:
  - take the next shape from the cycle, and move it 3 spaces apart from the top of the map
-}

step :: State -> State
step state@State {_settledShapes, _shapes = shape : nextShape : shapes} =
  if isValid state''
    then step state''
    else state''' -- processCutoff state'''
  where
    state' = push state
    state'' = fall state'
    state''' = state' {_shapes = initializeShape state''' nextShape : shapes, _settledShapes = (head . _shapes) state' : _settledShapes}

isDisjoint :: State -> Shape -> Bool
isDisjoint State {_settledShapes} shape = all (Set.disjoint shape) _settledShapes

isValid :: State -> Bool
isValid state@State {_settledShapes, _shapes = shape : _} = isDisjoint state shape && all withinBounds shape

push :: State -> State
push state@State {_shapes = shape : shapes, _jets = jet : jets, _settledShapes} = state {_shapes = shape' : shapes, _jets = jets}
  where
    movedShape = Set.map (move jet) shape
    shape' = if isDisjoint state movedShape && all withinBounds movedShape then movedShape else shape

fall :: State -> State
fall state@State {_shapes = shape : shapes} = state {_shapes = shape' : shapes}
  where
    shape' = Set.map moveDown shape

initializeShape :: State -> Shape -> Shape
initializeShape state = Set.map (\(x, y) -> (x + 2, y + topY state + 3 + 1))

topY :: State -> Int
topY State {_settledShapes} = maximum (-1 : (Set.findMax . Set.map snd <$> _settledShapes))

moveDown :: Coord -> Coord
moveDown (x, y) = (x, y - 1)

move :: Direction -> Coord -> Coord
move Left (x, y) = (x - 1, y)
move Right (x, y) = (x + 1, y)

withinBounds :: Coord -> Bool
withinBounds (x, y) = x >= 0 && x < width && y >= 0

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let jets = cycle input
  -- print input
  let state = State [] jets (initializeShape state (head shapes) : tail (cycle shapes))
  -- print $ head $ _shapes state
  let target = 1000000000000
  let stepsToStabilize = lcm (length input) (length shapes)
  print stepsToStabilize
  let cycles = target `div` stepsToStabilize
  -- print cycles
  let steps = iterate step state
  let nths = (\index -> topY (steps !! (stepsToStabilize * index)) + 1) <$> [1 ..]
  -- print $ take 20 nths
  let diffs = zipWith (-) (tail nths) nths
  print $ take 20 diffs
  let n = 7
  let (d, m) = cycles `quotRem` n
  print (d, m)
  print $ (sum (take n diffs) * d) + sum (take m diffs)

  -- let first = topY (steps !! stepsToStabilize) + 1
  -- let second = topY (steps !! (stepsToStabilize * 2)) + 1
  -- let third = topY (steps !! (stepsToStabilize * 3)) + 1
  -- let fourth = topY (steps !! (stepsToStabilize * 4)) + 1
  -- let fifth = topY (steps !! (stepsToStabilize * 5)) + 1
  -- let diff = second - first
  -- print (first, second - first, third - second, fourth - third, fifth - fourth)
  -- print (first, diff, cycles)
  -- print $ first + (diff * (cycles - 1))
  -- print $ first * cycles
  return ()

data Direction = Left | Right deriving (Show, Eq)

type Coord = (Int, Int)

type Shape = Set Coord

data State = State {_settledShapes :: [Shape], _jets :: [Direction], _shapes :: [Shape]}

parser :: Parser [Direction]
parser = many1 (Left <$ char '<' <|> Right <$ char '>')

main :: IO ()
main = do
  arg <- head . (++ ["all"]) <$> getArgs
  when (arg == "all" || arg == "part1") part1
  when (arg == "all" || arg == "part2") part2
