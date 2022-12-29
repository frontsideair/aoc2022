import Control.Applicative (Alternative ((<|>)))
import Control.Monad (when)
import Data.Function ((&))
import Data.Functor (($>))
import Data.Map (Map)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, mapMaybe)
import System.Environment (getArgs)
import Text.Parsec (char, count, eof, letter, string)
import Text.Parsec.Char (newline)
import Text.Parsec.Combinator (sepEndBy)
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number (int)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  -- print input
  print $ go (Map.fromList input) "root"
  return ()

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let map = Map.delete "humn" $ Map.fromList input
  let MathOp _ lhs rhs = map ! "root"
  -- print (lhs, rhs)
  let Just pair = (rhs,) <$> go map lhs <|> (lhs,) <$> go map rhs
  print $ go' map "humn" pair
  return ()

transform :: MonkeyId -> (MonkeyId, MonkeyJob) -> Maybe (MonkeyId, MonkeyJob)
transform _ (id, PlainNumber _) = Nothing
transform id (id', MathOp Add lhs rhs)
  | id == lhs = Just (id', MathOp Sub id' rhs)
  | id == rhs = Just (id', MathOp Sub id' lhs)
  | otherwise = Nothing
transform id (id', MathOp Sub lhs rhs)
  | id == lhs = Just (id', MathOp Add id' rhs)
  | id == rhs = Just (id', MathOp Sub lhs id')
  | otherwise = Nothing
transform id (id', MathOp Mul lhs rhs)
  | id == lhs = Just (id', MathOp Div id' rhs)
  | id == rhs = Just (id', MathOp Div id' lhs)
  | otherwise = Nothing
transform id (id', MathOp Div lhs rhs)
  | id == lhs = Just (id', MathOp Mul id' rhs)
  | id == rhs = Just (id', MathOp Div lhs id')
  | otherwise = Nothing

go :: Map MonkeyId MonkeyJob -> MonkeyId -> Maybe Int
go map id = case Map.lookup id map of
  Just (PlainNumber n) -> Just n
  Just (MathOp op lhs rhs) -> op' <$> lhs' <*> rhs'
    where
      op' = case op of
        Add -> (+)
        Sub -> (-)
        Mul -> (*)
        Div -> div
      lhs' = go map lhs
      rhs' = go map rhs
  Nothing -> Nothing

go' :: Map MonkeyId MonkeyJob -> MonkeyId -> (MonkeyId, Int) -> Maybe Int
go' map id pair =
  if id == fst pair
    then Just $ snd pair
    else ret
  where
    ret = case Map.findWithDefault (snd . fromJust $ x) id map' of
      (PlainNumber n) -> Just n
      (MathOp op lhs rhs) -> op' <$> lhs' <*> rhs'
        where
          op' = case op of
            Add -> (+)
            Sub -> (-)
            Mul -> (*)
            Div -> div
          lhs' = go' map' lhs pair
          rhs' = go' map' rhs pair
      where
        x =
          headMay $
            mapMaybe
              ( \(id', monkey) ->
                  case monkey of
                    mathOp@(MathOp op lhs rhs) -> transform id (id', mathOp)
                    PlainNumber _ -> Nothing
              )
              (Map.toList map)
        map' = case x of
          Just (id, _) -> Map.delete id map
          Nothing -> map

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x : _) = Just x

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
