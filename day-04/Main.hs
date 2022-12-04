import Text.Parsec (digit, many1, newline)
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (sepEndBy)
import Text.Parsec.String (Parser, parseFromFile)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  print $ length $ filter (\(l, r) -> l `contains` r || r `contains` l) input

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  print $ length $ filter (\(l, r) -> l `overlaps` r || r `overlaps` l) input

contains :: Range -> Range -> Bool
contains a b = start a <= start b && end a >= end b
  where
    start = fst
    end = snd

overlaps :: Range -> Range -> Bool
overlaps a b = (start a `within` b) || (end a `within` b)
  where
    start = fst
    end = snd
    within point range = point >= start range && point <= end range

type Range = (Int, Int)

type Pair = (Range, Range)

parser :: Parser [Pair]
parser = pair `sepEndBy` newline

pair :: Parser Pair
pair = do
  left <- range
  char ','
  right <- range
  return (left, right)

range :: Parser Range
range = do
  start <- number
  char '-'
  end <- number
  return (start, end)

number :: Parser Int
number = read <$> many1 digit

main :: IO ()
main = do
  part1
  part2