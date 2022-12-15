import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.List (find)
import System.Environment (getArgs)
import Text.Parsec (char, digit, eof, many1, string)
import Text.Parsec.Char (newline)
import Text.Parsec.Combinator (sepEndBy)
import Text.Parsec.String (Parser, parseFromFile)

targetY :: Int
targetY = 2000000

manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  -- print input
  let (xStart, xEnd) = (minimum $ leftmostX <$> input, maximum $ rightmostX <$> input)
  let coordsToCheck = (,targetY) <$> [xStart .. xEnd]
  -- print coordsToCheck
  print $
    length $
      filter
        (\coord -> any (\sensor@Sensor {beacon} -> sensor `disallowed` coord && beacon /= coord) input)
        coordsToCheck
  return ()

disallowed, allowed :: Sensor -> Coord -> Bool
disallowed Sensor {position, beacon} target = manhattan position target <= manhattan position beacon
allowed a = not . disallowed a

leftmostX :: Sensor -> Int
leftmostX Sensor {position = (x, y), beacon} = x - manhattan (x, y) beacon

rightmostX :: Sensor -> Int
rightmostX Sensor {position = (x, y), beacon} = x + manhattan (x, y) beacon

minCoord, maxCoord :: Int
minCoord = 0
maxCoord = 4000000

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let coordsToCheck = [(x, y) | x <- [minCoord .. maxCoord], y <- [minCoord .. maxCoord]]
  -- print coordsToCheck
  print $ tuningFrequency $ run input (minCoord, minCoord)
  return ()

tuningFrequency :: Coord -> Int
tuningFrequency (x, y) = x * 4000000 + y

run :: [Sensor] -> Coord -> Coord
run sensors (x, y) = case find (`disallowed` (x, y)) sensors of
  Nothing -> (x, y)
  Just sensor@Sensor {position = position@(sx, _), beacon} -> run sensors coord'
    where
      x' = numSteps
      coord' = if x' > maxCoord then (minCoord, y + 1) else (x', y)
      numSteps = 1 + sx + manhattan position beacon - abs (y - snd position)

type Coord = (Int, Int)

data Sensor = Sensor {position :: Coord, beacon :: Coord} deriving (Show)

parser :: Parser [Sensor]
parser = sensor `sepEndBy` newline <* eof

sensor :: Parser Sensor
sensor = Sensor <$> (string "Sensor at " *> coord) <*> (string ": closest beacon is at " *> coord)

coord :: Parser Coord
coord = do
  string "x="
  x <- int
  string ", y="
  y <- int
  return (x, y)

int :: Parser Int
int = do
  sign <- (char '-' >> return negate) <|> return id
  num <- many1 digit
  return $ sign $ read num

main :: IO ()
main = do
  arg <- head . (++ ["all"]) <$> getArgs
  when (arg == "all" || arg == "part1") part1
  when (arg == "all" || arg == "part2") part2
