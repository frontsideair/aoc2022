import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Functor.Identity (Identity)
import Data.List (find, findIndex, unfoldr)
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
    else state'''
  where
    state' = push state
    state'' = fall state'
    state''' = state' {_shapes = initializeShape state''' nextShape : shapes, _settledShapes = take 50 $ (head . _shapes) state' : _settledShapes}

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

findCycle' :: Eq a => [a] -> ([a], [a])
findCycle' xxs = fCycle xxs xxs
  where
    fCycle (x : xs) (_ : y : ys)
      | x == y = fStart xxs xs
      | otherwise = fCycle xs ys
    fCycle _ _ = (xxs, []) -- not cyclic
    fStart (x : xs) (y : ys)
      | x == y = ([], x : fLength x xs)
      | otherwise = let (as, bs) = fStart xs ys in (x : as, bs)
    fLength x (y : ys)
      | x == y = []
      | otherwise = y : fLength x ys

findCycle :: Eq a => [a] -> Maybe ([a], Int, Int)
findCycle lst =
  do
    l <- findCycleLength lst
    mu <- findIndex (uncurry (==)) $ zip lst (drop l lst)
    let c = take l $ drop mu lst
    return (c, l, mu)

findCycleLength :: Eq a => [a] -> Maybe Int
findCycleLength [] = Nothing
findCycleLength (x : xs) =
  let loop _ _ _ [] = Nothing
      loop pow lam x (y : ys)
        | x == y = Just lam
        | pow == lam = loop (2 * pow) 1 y ys
        | otherwise = loop pow (1 + lam) x ys
   in loop 1 1 x xs

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let jets = cycle input
  -- print input
  let state = State [] jets (initializeShape state (head shapes) : tail (cycle shapes))
  -- print $ head $ _shapes state
  let target = 1000000000000
  let stepsToStabilize = 320 -- magic number
  -- print stepsToStabilize
  let cycles = target `div` stepsToStabilize
  -- print cycles
  let steps = iterate step state
  let nths = (\index -> topY (steps !! (stepsToStabilize * index)) + 1) <$> [1 ..]
  -- let nths = (+ 1) . topY <$> iterate step state
  -- print $ take 20 nths
  let diffs = zipWith (-) (tail nths) nths
  -- print $ take 1000 diffs
  let (xs, ys) = findCycle' diffs
  -- let (xs, ys) :: ([Int], [Int]) = ([37, 29, 38, 33, 32], [31, 30, 34, 35, 35, 35, 31, 40, 27, 31, 28, 27, 36, 29, 34, 32, 30, 34, 28, 36, 32, 27, 31, 36, 25, 27, 33, 32, 34, 32, 35, 36, 30, 26, 35, 38, 30, 30, 33, 39, 34, 29, 34, 24, 37, 28, 30, 28, 25, 32, 36, 29, 29, 27, 36, 28, 34, 23, 30, 33, 31, 31, 23, 27, 32, 30, 33, 30, 31, 37, 27, 31, 39, 26, 34, 43, 32, 34, 36, 30, 33, 35, 35, 36, 28, 33, 32]) -- debug
  -- print (xs, ys)
  let (q, r) = (cycles - 1 - length xs) `quotRem` length ys
  print (head nths + sum xs + sum ys * q + sum (take r ys))

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
