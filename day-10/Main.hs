{-# LANGUAGE NamedFieldPuns #-}

import Control.Applicative (optional, (<|>))
import Control.Monad (when, (>=>))
import Data.Functor (($>))
import Data.List (find, tails)
import System.Environment (getArgs)
import Text.Parsec (char, digit, eof, many1, string)
import Text.Parsec.Char (newline)
import Text.Parsec.Combinator (sepEndBy)
import Text.Parsec.String (Parser, parseFromFile)
import Prelude hiding (cycle)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  -- print input
  let states = scanl step initialState input
  print $ sum $ (\n -> strength n $ atCycle states n) <$> [20, 60 .. 220]
  return ()

atCycle :: [State] -> Int -> State
atCycle states n = last $ takeWhile (\s -> cycle s < n) states

strength :: Int -> State -> Int
strength n (State x cycle) = x * n

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let states = scanl step initialState input
  print `traverse` aperture 40 ((\n -> overlaps n $ atCycle states n) <$> [1 .. 240])
  -- print $ (\n -> let State {x} = atCycle states n in (n `mod` 40, [x, x + 1, x + 2])) <$> [161 .. 200]
  return ()

overlaps :: Int -> State -> Char
overlaps n State {x} = if n `mod` 40 `elem` ((`mod` 40) <$> [x, x + 1, x + 2]) then '#' else '.'

aperture :: Int -> [a] -> [[a]]
aperture n [] = []
aperture n xs = take n xs : aperture n (drop n xs)

data State = State {x :: Int, cycle :: Int} deriving (Show)

initialState :: State
initialState = State 1 0

step :: State -> Instruction -> State
step (State x cycle) Noop = State x (cycle + 1)
step (State x cycle) (AddX n) = State (x + n) (cycle + 2)

data Instruction = Noop | AddX Int deriving (Show)

parser :: Parser [Instruction]
parser = instruction `sepEndBy` newline <* eof

instruction :: Parser Instruction
instruction = string "noop" $> Noop <|> AddX <$> (string "addx " *> int)

int :: Parser Int
int = sign <*> digits
  where
    sign = (char '-' >> return negate) <|> (optional (char '+') >> return id)
    digits = read <$> many1 digit

main :: IO ()
main = do
  arg <- head . (++ ["all"]) <$> getArgs
  when (arg == "all" || arg == "part1") part1
  when (arg == "all" || arg == "part2") part2
