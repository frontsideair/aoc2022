{-# LANGUAGE NamedFieldPuns #-}

import Control.Applicative ((<|>))
import Data.Foldable (foldl')
import Data.Functor (($>))
import Data.List (transpose)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Strict ((!))
import Data.Maybe (catMaybes)
import Text.Parsec (char, count, digit, endBy, eof, letter, newline, oneOf, optionMaybe, sepBy, sepBy1, sepEndBy)
import Text.Parsec.Char (string)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Prim (try)
import Text.Parsec.String (Parser, parseFromFile)

part1 :: IO ()
part1 = do
  (stacks, moves) <- parseFromFile parser "input.txt" >>= either (error . show) return
  let result = foldl' step stacks moves
  print $ Map.elems $ head <$> result

part2 :: IO ()
part2 = do
  (stacks, moves) <- parseFromFile parser "input.txt" >>= either (error . show) return
  let result = foldl' step' stacks moves
  print $ Map.elems $ head <$> result

step :: Stacks -> Move -> Stacks
step stacks Move {amount, from, to} = stacks''
  where
    (toMove, fromStack) = splitAt amount $ stacks ! from
    stacks' = Map.insert from fromStack stacks
    stacks'' = Map.adjust (reverse toMove ++) to stacks'

step' :: Stacks -> Move -> Stacks
step' stacks Move {amount, from, to} = stacks''
  where
    (toMove, fromStack) = splitAt amount $ stacks ! from
    stacks' = Map.insert from fromStack stacks
    stacks'' = Map.adjust (toMove ++) to stacks'

type Stacks = Map Int Stack

type Stack = [Char]

data Move = Move {amount :: Int, from :: Int, to :: Int} deriving (Show)

parser :: Parser (Stacks, [Move])
parser = do
  stacks <- stacksParser
  count 2 newline
  moves <- moveParser `sepEndBy` newline
  eof
  return (stacks, moves)

stacksParser :: Parser Stacks
stacksParser = do
  rows <- row `sepEndBy` newline
  space
  indices <- number `sepBy` try (count 3 space)
  space
  let cols = catMaybes <$> transpose rows
  return $ Map.fromList $ zip indices cols

row :: Parser [Maybe Char]
row = try $ (Just <$> (char '[' *> letter <* char ']') <|> (Nothing <$ count 3 space)) `sepBy` space

moveParser :: Parser Move
moveParser = Move <$> (string "move " *> number) <*> (string " from " *> number) <*> (string " to " *> number)

number :: Parser Int
number = read <$> many1 digit

space :: Parser Char
space = char ' '

main :: IO ()
main = do
  part1
  part2