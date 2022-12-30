import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Foldable (foldl')
import Data.Functor (($>))
import Data.Map (Map)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)
import Text.Parsec (many1, newline, sepEndBy)
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (eof)
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number (int)

startingPoint :: Map Coord Cell -> Coord
startingPoint map = head $ Map.keys $ Map.filter (== Floor) map

startingDirection :: Direction
startingDirection = East

part1 :: IO ()
part1 = do
  (map, path) <- parseFromFile parser "input.txt" >>= either (error . show) return
  let state = (startingPoint map, startingDirection)
  -- print (map, path, state)
  print $ score $ foldl' (step jump map) state path
  return ()

step :: (Map Coord Cell -> (Coord, Direction) -> (Coord, Direction)) -> Map Coord Cell -> (Coord, Direction) -> Instruction -> (Coord, Direction)
step _ _ (coord, direction) TurnLeft = (coord, turnLeft direction)
step _ _ (coord, direction) TurnRight = (coord, turnRight direction)
step jump map (coord, direction) (Move steps) = go steps (coord, direction)
  where
    go n (coord, direction) = if n == 0 || map ! coord' == Wall then (coord, direction) else go (n - 1) (coord', direction')
      where
        (coord', direction') = if Map.member candidate map then (candidate, direction) else jump map (coord, direction)
        candidate = move coord direction

move :: Coord -> Direction -> Coord
move (y, x) North = (y - 1, x)
move (y, x) East = (y, x + 1)
move (y, x) South = (y + 1, x)
move (y, x) West = (y, x - 1)

jump :: Map Coord Cell -> (Coord, Direction) -> (Coord, Direction)
jump map ((y, x), direction) = (coord', direction)
  where
    keys = Map.keys map
    coord' = case direction of
      North -> last $ filter ((== x) . snd) keys
      East -> head $ filter ((== y) . fst) keys
      South -> head $ filter ((== x) . snd) keys
      West -> last $ filter ((== y) . fst) keys

{-
  TTRR
  TTRR
  FF
  FF
LLBB
LLBB
HH
HH
-}

jump' :: Map Coord Cell -> (Coord, Direction) -> (Coord, Direction)
jump' map ((y, x), direction) = (coord', direction')
  where
    currentFace = findCurrentFace (y, x)
    (_, direction', coord') = case (currentFace, direction) of
      (TopFace, North) -> (HindFace, East, (x + 100, y)) -- (1, 51) -> (151, 1), (1, 100) -> (200, 1)
      (TopFace, West) -> (LeftFace, East, (151 - y, x - 50)) -- (1, 51) -> (150, 1), (50, 51) -> (101, 1)
      (RightFace, North) -> (HindFace, North, (y + 199, x - 100)) -- (1, 101) -> (200, 1), (1, 150) -> (200, 50)
      (RightFace, East) -> (BottomFace, West, (151 - y, x - 50)) -- (1, 150) -> (150, 100), (50, 150) -> (101, 100)
      (RightFace, South) -> (FrontFace, West, (x - 50, y + 50)) -- (50, 101) -> (51, 100), (50, 150) -> (100, 100)
      (FrontFace, West) -> (LeftFace, South, (x + 50, y - 50)) -- (51, 51) -> (101, 1), (100, 51) -> (101, 50)
      (FrontFace, East) -> (RightFace, North, (x - 50, y + 50)) -- (51, 100) -> (50, 101), (100, 100) -> (50, 150)
      (LeftFace, West) -> (TopFace, East, (151 - y, x + 50)) -- (101, 1) -> (50, 51), (150, 1) -> (1, 51)
      (LeftFace, North) -> (FrontFace, East, (x + 50, y - 50)) -- (101, 1) -> (51, 51), (101, 50) -> (100, 51)
      (BottomFace, South) -> (HindFace, West, (x + 100, y - 100)) -- (150, 51) -> (151, 50), (150, 100) -> (200, 50)
      (BottomFace, East) -> (RightFace, West, (151 - y, x + 50)) -- (101, 100) -> (50, 150), (150, 100) -> (1, 150)
      (HindFace, West) -> (TopFace, South, (x, y - 100)) -- (151, 1) -> (1, 51), (200, 1) -> (1, 100)
      (HindFace, South) -> (RightFace, South, (y - 199, x + 100)) -- (200, 1) -> (1, 101), (200, 50) -> (1, 150)
      (HindFace, East) -> (BottomFace, North, (x + 100, y - 100)) -- (151, 50) -> (150, 51), (200, 50) -> (150, 100)
      _ -> error "not found"

data Face = TopFace | FrontFace | RightFace | HindFace | LeftFace | BottomFace deriving (Show, Eq)

findCurrentFace :: Coord -> Face
findCurrentFace coord
  | coord `within` (1, 51) = TopFace
  | coord `within` (1, 101) = RightFace
  | coord `within` (51, 51) = FrontFace
  | coord `within` (101, 1) = LeftFace
  | coord `within` (101, 51) = BottomFace
  | coord `within` (151, 1) = HindFace
  | otherwise = error "not found"

within :: Coord -> Coord -> Bool
within (y, x) (yMin, xMin) = y `w` (yMin, yMin + 50) && x `w` (xMin, xMin + 50)
  where
    w x (min, max) = x >= min && x < max

score :: (Coord, Direction) -> Int
score (coord, dir) = scoreCoord coord + scoreDir dir
  where
    scoreCoord (y, x) = y * 1000 + x * 4
    scoreDir East = 0
    scoreDir South = 1
    scoreDir West = 2
    scoreDir North = 3

part2 :: IO ()
part2 = do
  (map, path) <- parseFromFile parser "input.txt" >>= either (error . show) return
  let state = (startingPoint map, startingDirection)
  print $ score $ foldl' (step jump' map) state path
  return ()

parser :: Parser (Map Coord Cell, [Instruction])
parser = do
  map <- mapParser
  newline
  path <- pathParser
  newline
  eof
  return (map, path)

pathParser :: Parser [Instruction]
pathParser = many1 instruction
  where
    instruction = char 'L' $> TurnLeft <|> char 'R' $> TurnRight <|> Move <$> int

mapParser :: Parser (Map Coord Cell)
mapParser = (Map.fromList . filter ((/= Space) . snd) . concat) . zipWith (\index row -> (\(index', cell) -> ((index, index'), cell)) <$> row) [1 ..] <$> (row `sepEndBy` newline)
  where
    row = zip [1 ..] <$> many1 cell
    cell = char ' ' $> Space <|> char '.' $> Floor <|> char '#' $> Wall

data Direction = North | East | South | West deriving (Show, Eq)

data Instruction = TurnLeft | TurnRight | Move Int deriving (Show, Eq)

data Cell = Space | Floor | Wall deriving (Show, Eq)

type Coord = (Int, Int)

turnLeft, turnRight :: Direction -> Direction
turnLeft North = West
turnLeft East = North
turnLeft South = East
turnLeft West = South
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

main :: IO ()
main = do
  arg <- head . (++ ["all"]) <$> getArgs
  when (arg == "all" || arg == "part1") part1
  when (arg == "all" || arg == "part2") part2
