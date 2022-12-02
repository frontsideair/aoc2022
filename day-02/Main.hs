import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.List (sort, sortOn)
import Data.Ord (Down (Down))
import Text.Parsec (char, digit, many1, sepBy, sepBy1, sepEndBy, space)
import Text.Parsec.Char (newline)
import Text.Parsec.String (Parser, parseFromFile)
import Prelude hiding (round)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  print $ sum $ score <$> input

data RPS = Rock | Paper | Scissors deriving (Eq, Ord, Show)

type Round = (RPS, RPS)

parser :: Parser [Round]
parser = do
  round `sepEndBy` newline

round :: Parser Round
round = do
  opponent <- (char 'A' $> Rock) <|> (char 'B' $> Paper) <|> (char 'C' $> Scissors)
  space
  self <- (char 'X' $> Rock) <|> (char 'Y' $> Paper) <|> (char 'Z' $> Scissors)
  return (opponent, self)

score :: Round -> Int
score round@(_, Rock) = 1 + result round
score round@(_, Paper) = 2 + result round
score round@(_, Scissors) = 3 + result round

result :: Round -> Int
result (Rock, Paper) = win
result (Paper, Scissors) = win
result (Scissors, Rock) = win
result (Rock, Scissors) = lose
result (Paper, Rock) = lose
result (Scissors, Paper) = lose
result _ = draw

win, lose, draw :: Int
win = 6
draw = 3
lose = 0

data Outcome = Win | Draw | Lose deriving (Eq, Ord, Show)

type Round' = (RPS, Outcome)

parser' :: Parser [Round']
parser' = do
  round' `sepEndBy` newline

round' :: Parser Round'
round' = do
  opponent <- (char 'A' $> Rock) <|> (char 'B' $> Paper) <|> (char 'C' $> Scissors)
  space
  outcome <- (char 'X' $> Lose) <|> (char 'Y' $> Draw) <|> (char 'Z' $> Win)
  return (opponent, outcome)

moveToAchieveOutcome :: Round' -> RPS
moveToAchieveOutcome (Rock, Win) = Paper
moveToAchieveOutcome (Paper, Win) = Scissors
moveToAchieveOutcome (Scissors, Win) = Rock
moveToAchieveOutcome (Rock, Lose) = Scissors
moveToAchieveOutcome (Paper, Lose) = Rock
moveToAchieveOutcome (Scissors, Lose) = Paper
moveToAchieveOutcome (rps, Draw) = rps

transformRound :: Round' -> Round
transformRound (opponent, outcome) = (opponent, moveToAchieveOutcome (opponent, outcome))

part2 :: IO ()
part2 = do
  input <- parseFromFile parser' "input.txt" >>= either (error . show) return
  let rounds = transformRound <$> input
  print $ sum $ score <$> rounds

main :: IO ()
main = do
  part1
  part2