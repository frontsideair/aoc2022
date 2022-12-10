{-# LANGUAGE NamedFieldPuns #-}

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Foldable (foldl')
import Data.Functor (($>))
import qualified Data.List as List
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
  let state = createState 1
  let finalState = foldl' step state input
  print $ length $ tailHistory finalState
  return ()

step :: State -> Direction -> State
step state@State {head, tail, tailHistory} dir = state {head = head', tail = tail', tailHistory = tailHistory'}
  where
    head' = moveHead dir head
    tail' = List.unfoldr f (head', tail)
    tailHistory' = Set.insert (List.last tail') tailHistory

f :: (Coord, [Coord]) -> Maybe (Coord, (Coord, [Coord]))
f (head, []) = Nothing
f (head, tail : rest) = Just (tail', (tail', rest))
  where
    tail' = moveTail head tail

index :: Int -> [a] -> Maybe a
index i xs = if length xs > i then Just (xs !! i) else Nothing

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let state = createState 9
  let finalState = foldl' step state input
  print $ length $ tailHistory finalState
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

data Direction = Up | Right | Down | Left deriving (Show, Eq)

type Coord = (Int, Int)

data State = State
  { head :: Coord,
    tail :: [Coord],
    tailHistory :: Set Coord
  }
  deriving (Show)

createState :: Int -> State
createState size = State {head, tail = replicate size head, tailHistory = Set.singleton head}
  where
    head = (1, 1)

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
