{-# LANGUAGE NamedFieldPuns #-}

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Foldable (foldl')
import Data.Functor (($>))
import qualified Data.List as List
import qualified Data.Matrix as Matrix
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs)
import Text.Parsec (char, digit, many1, newline, sepEndBy)
import Text.Parsec.Char (space)
import Text.Parsec.Combinator (eof)
import Text.Parsec.String (Parser, parseFromFile)
import Prelude hiding (Left, Right, head, tail)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  -- print input
  let state = State {head = (1, 1), tail = (1, 1), tailHistory = Set.singleton (1, 1)}
  let finalState = foldl' step state input
  -- print $ Matrix.matrix 6 6 (`Set.member` tailHistory finalState)
  print $ length $ tailHistory finalState
  -- print (tail . foldl' step state <$> List.inits input)
  return ()

step :: State -> Direction -> State
step state@State {head, tail, tailHistory} dir = state {head = head', tail = tail', tailHistory = tailHistory'}
  where
    head' = moveHead dir head
    tail' = moveTail head' tail
    tailHistory' = Set.insert tail tailHistory

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  -- print $
  return ()

moveHead :: Direction -> Coord -> Coord
moveHead Up (x, y) = (x, y + 1)
moveHead Right (x, y) = (x + 1, y)
moveHead Down (x, y) = (x, y - 1)
moveHead Left (x, y) = (x - 1, y)

moveTail :: Coord -> Coord -> Coord
moveTail head@(x, y) tail@(x', y')
  | abs (x - x') <= 1 && abs (y - y') <= 1 = tail
  | x == x' = (x', y' + signum (y - y'))
  | y == y' = (x' + signum (x - x'), y')
  | otherwise = (x' + signum (x - x'), y' + signum (y - y'))

-- manhattan :: Coord -> Coord -> Int
-- manhattan (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

data Direction = Up | Right | Down | Left deriving (Show, Eq)

type Coord = (Int, Int)

data State = State
  { head :: Coord,
    tail :: Coord,
    tailHistory :: Set Coord
  }
  deriving (Show)

parser :: Parser [Direction]
parser = concat <$> movement `sepEndBy` newline <* eof

movement :: Parser [Direction]
movement = do
  dir <- direction
  space
  dist <- number
  return $ replicate dist dir

direction :: Parser Direction
direction = char 'U' $> Up <|> char 'R' $> Right <|> char 'D' $> Down <|> char 'L' $> Left

number :: Parser Int
number = read <$> many1 digit

main :: IO ()
main = do
  arg <- List.head . (++ ["all"]) <$> getArgs
  when (arg == "all" || arg == "part1") part1
  when (arg == "all" || arg == "part2") part2
