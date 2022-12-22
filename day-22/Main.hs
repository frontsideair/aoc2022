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
  print $ score $ foldl' (step map) state path
  return ()

step :: Map Coord Cell -> (Coord, Direction) -> Instruction -> (Coord, Direction)
step map (coord, direction) TurnLeft = (coord, turnLeft direction)
step map (coord, direction) TurnRight = (coord, turnRight direction)
step map (coord, direction) (Move steps) = (go steps coord, direction)
  where
    go n coord = if n == 0 || map ! coord' == Wall then coord else go (n - 1) coord' where coord' = proceed map direction coord

proceed :: Map Coord Cell -> Direction -> Coord -> Coord
proceed map direction (y, x) = case direction of
  North -> if Map.member candidate map then candidate else last $ filter ((== x) . snd) $ Map.keys map where candidate = (y - 1, x)
  East -> if Map.member candidate map then candidate else head $ filter ((== y) . fst) $ Map.keys map where candidate = (y, x + 1)
  South -> if Map.member candidate map then candidate else head $ filter ((== x) . snd) $ Map.keys map where candidate = (y + 1, x)
  West -> if Map.member candidate map then candidate else last $ filter ((== y) . fst) $ Map.keys map where candidate = (y, x - 1)

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
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
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
