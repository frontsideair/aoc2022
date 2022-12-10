import Control.Applicative (optional, (<|>))
import Control.Monad (when, (>=>))
import Data.Functor (($>))
import Data.List (find)
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
  print $ sum $ atCycle states <$> [20, 60 .. 220]
  return ()

atCycle :: [State] -> Int -> Int
atCycle states n = strength n <$> last $ takeWhile (\s -> cycle s < n) states
  where
    strength n (State x cycle) = x * n

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  return ()

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
