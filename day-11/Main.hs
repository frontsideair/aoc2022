{-# LANGUAGE NamedFieldPuns #-}

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Function ((&))
import Data.Functor (($>))
import Data.List (foldl', sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Strict ((!))
import Data.Ord (Down (Down))
import System.Environment (getArgs)
import Text.Parsec (digit, many1, sepBy, sepEndBy, spaces, string)
import Text.Parsec.Char (newline)
import Text.Parsec.Combinator (eof)
import Text.Parsec.String (Parser, parseFromFile)
import Prelude hiding (round)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  -- print $ heldItems <$> input
  -- print (($ 13) . operation <$> input)
  -- print $ heldItems <$> turn (`div` 3) input 0
  -- print $ heldItems <$> round (`div` 3) input
  -- print $ inspected <$> iterate (round (`div` 3)) input !! 20
  print $ monkeyBusiness $ iterate (round (`div` 3)) input !! 20
  return ()

turn :: (Int -> Int) -> Map Int Monkey -> Int -> Map Int Monkey
turn worryManager monkeys index = case monkeys ! index of
  Monkey {heldItems = []} -> monkeys
  monkey@(Monkey operation (Test condition ifTrue ifFalse) (item : rest) inspected) -> turn worryManager monkeys' index
    where
      worryLevel = worryManager $ operation item
      conditionResult = worryLevel `mod` condition == 0
      monkeyToThrow = if conditionResult then ifTrue else ifFalse
      monkey' = monkey {heldItems = rest, inspected = inspected + 1}
      monkeys' =
        monkeys
          & Map.adjust (\monkey@Monkey {heldItems} -> monkey {heldItems = heldItems ++ [worryLevel]}) monkeyToThrow
          & Map.insert index monkey'

round :: (Int -> Int) -> Map Int Monkey -> Map Int Monkey
round worryManager monkeys = foldl' (turn worryManager) monkeys (Map.keys monkeys)

monkeyBusiness :: Map Int Monkey -> Int
monkeyBusiness monkeys = product $ take 2 $ sortOn Down $ inspected <$> Map.elems monkeys

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let modBy = product $ condition . test <$> Map.elems input
  -- print $ inspected <$> iterate (round (`mod` modBy)) input !! 1000
  print $ monkeyBusiness $ iterate (round (`mod` modBy)) input !! 10000
  return ()

data Test = Test {condition :: Int, ifTrue :: Int, ifFalse :: Int} deriving (Show)

data Monkey = Monkey {operation :: Int -> Int, test :: Test, heldItems :: [Int], inspected :: Int}

parser :: Parser (Map Int Monkey)
parser = Map.fromList <$> monkey `sepEndBy` newline <* eof

monkey :: Parser (Int, Monkey)
monkey = do
  string "Monkey "
  index <- decimal
  string ":"
  spaces
  string "Starting items: "
  heldItems <- decimal `sepBy` string ", "
  spaces
  string "Operation: new = old "
  operation <- op
  spaces
  string "Test: divisible by "
  condition <- decimal
  spaces
  string "If true: throw to monkey "
  ifTrue <- decimal
  spaces
  string "If false: throw to monkey "
  ifFalse <- decimal
  newline
  return (index, Monkey operation (Test condition ifTrue ifFalse) heldItems 0)

decimal :: Parser Int
decimal = read <$> many1 digit

op :: Parser (Int -> Int)
op = do
  f <- string "* " $> (*) <|> string "+ " $> (+)
  f <$> decimal <|> string "old" $> (\a -> a `f` a)

main :: IO ()
main = do
  arg <- head . (++ ["all"]) <$> getArgs
  when (arg == "all" || arg == "part1") part1
  when (arg == "all" || arg == "part2") part2
