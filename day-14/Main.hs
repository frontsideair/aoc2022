{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad (when)
import Data.Foldable (find, fold, foldl', maximumBy, minimumBy)
import Data.List (tails)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs)
import Text.Parsec (digit, eof, many1, newline, sepBy1, string)
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (sepEndBy)
import Text.Parsec.String (Parser, parseFromFile)

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

start :: Coord
start = (500, 0)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  -- print input
  -- print $ range 10 1
  -- print $ line (1, 10) (1, 1)
  -- print $ pairs [1 .. 10]
  let bottom = maximum (snd <$> Set.toList input) + 2
  -- print bottom
  print $ Set.size . sands <$> find ((> bottom) . snd . current) (iterate (step (const True)) (State input Set.empty start))
  return ()

step :: (Coord -> Bool) -> State -> State
step cond state@State {rocks, sands, current} =
  case filter (\neighbor -> neighbor `Set.notMember` rocks && neighbor `Set.notMember` sands && cond neighbor) (neighbors current) of
    [] -> state {sands = Set.insert current sands, current = start}
    current' : _ -> state {current = current'}

neighbors :: Coord -> [Coord]
neighbors (x, y) = [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let bottom = maximum (snd <$> Set.toList input) + 2
  -- print bottom
  print $ Set.size . sands <$> find ((start `Set.member`) . sands) (iterate (step ((/= bottom) . snd)) (State input Set.empty start))
  return ()

line :: Coord -> Coord -> Set Coord
line (x1, y1) (x2, y2) = Set.fromList $ if x1 == x2 then (x1,) <$> range y1 y2 else (,y1) <$> range x1 x2

range :: (Ord a, Enum a) => a -> a -> [a]
range x y = if x < y then [x .. y] else [y .. x]

type Coord = (Int, Int)

data State = State {rocks :: Set Coord, sands :: Set Coord, current :: Coord} deriving (Show)

parser :: Parser (Set Coord)
parser = fold <$> path `sepEndBy` newline <* eof

path :: Parser (Set Coord)
path = do
  coords <- coord `sepBy1` string " -> "
  return $ foldMap (uncurry line) (pairs coords)

coord :: Parser Coord
coord = (,) <$> (int <* char ',') <*> int

int :: Parser Int
int = read <$> many1 digit

main :: IO ()
main = do
  arg <- head . (++ ["all"]) <$> getArgs
  when (arg == "all" || arg == "part1") part1
  when (arg == "all" || arg == "part2") part2
