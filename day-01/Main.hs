import Data.List (sort, sortOn)
import Data.Ord (Down (Down))
import Text.Parsec (digit, many1, sepBy, sepBy1, sepEndBy)
import Text.Parsec.Char (newline)
import Text.Parsec.String (Parser, parseFromFile)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  print $ maximum $ sum <$> input

parser :: Parser [[Int]]
parser = do
  calorie `sepBy` newline

calorie :: Parser [Int]
calorie = number `sepEndBy` newline

number :: Parser Int
number = read <$> many1 digit

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  print $ sum $ take 3 $ sortOn Down $ sum <$> input

main :: IO ()
main = do
  part1
  part2