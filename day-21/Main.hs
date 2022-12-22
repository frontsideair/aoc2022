import Control.Applicative (Alternative ((<|>)))
import Control.Monad (when)
import Data.Function ((&))
import Data.Functor (($>))
import Data.Map (Map)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Debug.Trace (traceShow)
import System.Environment (getArgs)
import Text.Parsec (char, count, eof, letter, string)
import Text.Parsec.Char (newline)
import Text.Parsec.Combinator (sepEndBy)
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec (try)
import Text.ParserCombinators.Parsec.Number (decimal, int)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  -- print input
  print $ go' (Map.fromList input) "root"
  return ()

f (PlainNumber n) _ = PlainNumber n
f _ (PlainNumber n) = PlainNumber n
f a b = b

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let map = Map.delete "humn" $ Map.fromList input
  let reverseMap = Map.fromListWith f $ concatMap transform $ Map.toList map
  -- print $ go map <$> ["nntd", "zzfl", "lgwz"]
  let MathOp _ lhs rhs = map ! "root"
  let Just (id, n) = (rhs,) <$> go' map lhs <|> (lhs,) <$> go' map rhs
  print (id, n)
  let reverseMap' = Map.insert id (PlainNumber n) $ Map.fromListWith f $ concatMap transform $ Map.toList map
  -- print `traverse` (go map <$> ["nntd", "zzfl", "lgwz"])
  print "-----------"
  -- print `traverse` (go reverseMap' <$> ["nntd", "lgwz"])
  -- print $ go reverseMap' <$> ["nntd", "zzfl", "lgwz"]
  -- print $ go reverseMap' <$> ["rvrh", "hzgl"]
  -- print reverseMap'
  -- print $ Map.lookup "pppw" reverseMap
  -- print $ Map.lookup "sjmn" reverseMap
  -- let Just root = eval' map "root"
  -- print root
  -- let reverseMap = Map.insert "root" (PlainNumber root) reverseMap
  -- let lhs = ("rvrh",) <$> eval' map "rvrh"
  -- let rhs = ("hzgl",) <$> eval' map "hzgl"
  -- print $ lhs <|> rhs
  -- let Just (id, n) = lhs <|> rhs
  -- print (id, n)
  -- let reverseMap' = Map.insert "root" (PlainNumber n) $ Map.insert "hzgl" (PlainNumber n) $ Map.insert "rvrh" (PlainNumber n) reverseMap
  -- print reverseMap'
  print $ eval map reverseMap' "humn"
  -- print $ eval' map reverseMap "humn"
  -- let input' = Map.adjust (\(MathOperation _ lhs rhs) -> MathOperation Subtraction lhs rhs) "root" input
  -- let diff = eval input' "root"
  -- print diff
  -- print $ eval input "pppw"
  -- print $ eval input "sjmn"
  -- let input'' = Map.adjust (\(PlainNumber n) -> PlainNumber 301) "humn" input'
  -- print $ eval input'' "root"
  return ()

transform :: (MonkeyId, MonkeyJob) -> [(MonkeyId, MonkeyJob)]
transform (id, PlainNumber n) = [(id, PlainNumber n)]
transform (id, MathOp Add lhs rhs) = [(lhs, MathOp Sub id rhs), (rhs, MathOp Sub id lhs)]
transform (id, MathOp Sub lhs rhs) = [(lhs, MathOp Add id rhs), (rhs, MathOp Sub lhs id)]
transform (id, MathOp Mul lhs rhs) = [(lhs, MathOp Div id rhs), (rhs, MathOp Div id lhs)]
transform (id, MathOp Div lhs rhs) = [(lhs, MathOp Mul id rhs), (rhs, MathOp Div lhs id)]

-- eval :: Map MonkeyId MonkeyJob -> MonkeyId -> Maybe Int
eval map reverseMap id = go map id <|> go reverseMap id
  where
    go map id = case Map.lookup id map of
      Just (PlainNumber n) -> Just n
      Just (MathOp op lhs rhs) -> op' <$> lhs' <*> rhs' -- & traceShow (id, lhs, rhs)
        where
          op' = case op of
            Add -> (+)
            Sub -> (-)
            Mul -> (*)
            Div -> div
          lhs' = eval map reverseMap lhs
          rhs' = eval map reverseMap rhs
      Nothing -> Nothing

go' map id = case Map.lookup id map of
  Just (PlainNumber n) -> Just n
  Just (MathOp op lhs rhs) -> op' <$> lhs' <*> rhs' -- & traceShow (id, lhs, rhs)
    where
      op' = case op of
        Add -> (+)
        Sub -> (-)
        Mul -> (*)
        Div -> div
      lhs' = go' map lhs
      rhs' = go' map rhs
  Nothing -> Nothing

type MonkeyId = String

data Op = Add | Sub | Mul | Div deriving (Show)

data MonkeyJob = PlainNumber Int | MathOp Op MonkeyId MonkeyId deriving (Show)

parser :: Parser [(MonkeyId, MonkeyJob)]
parser = monkey `sepEndBy` newline <* eof

monkey :: Parser (MonkeyId, MonkeyJob)
monkey = do
  id <- monkeyId
  string ": "
  job <- PlainNumber <$> int <|> mathOperation
  return (id, job)

mathOperation :: Parser MonkeyJob
mathOperation = do
  lhs <- monkeyId
  char ' '
  op <- char '+' $> Add <|> char '-' $> Sub <|> char '*' $> Mul <|> char '/' $> Div
  char ' '
  MathOp op lhs <$> monkeyId

monkeyId :: Parser MonkeyId
monkeyId = count 4 letter

main :: IO ()
main = do
  arg <- head . (++ ["all"]) <$> getArgs
  when (arg == "all" || arg == "part1") part1
  when (arg == "all" || arg == "part2") part2
