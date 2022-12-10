import Control.Monad (when)
import Data.Matrix (Matrix, (!))
import qualified Data.Matrix as Matrix
import System.Environment (getArgs)
import Text.Parsec (count, digit, eof, many1)
import Text.Parsec.Char (newline)
import Text.Parsec.Combinator (sepEndBy)
import Text.Parsec.String (Parser, parseFromFile)
import Prelude hiding (Left, Right)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  -- print input
  let visible = Matrix.mapPos (\pos a -> any (< a) [get input (dir, pos) | dir <- [Up .. Left]]) input
  print $ length $ filter (== True) (Matrix.toList visible)
  return ()

data Height = Height {_top :: Int, _right :: Int, _bottom :: Int, _left :: Int} deriving (Show)

data Dir = Up | Right | Down | Left deriving (Show, Eq, Ord, Enum, Bounded)

get :: Matrix Int -> (Dir, (Int, Int)) -> Int
get matrix (dir, (x, y)) = case Matrix.safeGet x' y' matrix of
  Nothing -> -1
  Just current -> max (get matrix (dir, (x', y'))) current
  where
    (x', y') = move dir (x, y)

move :: Dir -> (Int, Int) -> (Int, Int)
move dir (x, y) = case dir of
  Up -> (x - 1, y)
  Right -> (x, y + 1)
  Down -> (x + 1, y)
  Left -> (x, y - 1)

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  -- print input
  let scenicScores = Matrix.mapPos (\pos a -> product [get' input (a, dir, pos) | dir <- [Up .. Left]]) input
  print $ maximum (Matrix.toList scenicScores)
  return ()

get' :: Matrix Int -> (Int, Dir, (Int, Int)) -> Int
get' matrix (n, dir, (x, y)) = case Matrix.safeGet x' y' matrix of
  Nothing -> 0
  Just current -> if n > current then 1 + get' matrix (n, dir, (x', y')) else 1
  where
    (x', y') = move dir (x, y)

parser :: Parser (Matrix Int)
parser = Matrix.fromLists <$> line `sepEndBy` newline <* eof

line :: Parser [Int]
line = many1 (read <$> count 1 digit)

main :: IO ()
main = do
  arg <- head . (++ ["all"]) <$> getArgs
  when (arg == "all" || arg == "part1") part1
  when (arg == "all" || arg == "part2") part2
