import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.List (elemIndex, elemIndices, findIndices, sort)
import System.Environment (getArgs)
import Text.Parsec (between, digit, many1, newline, sepEndBy)
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (eof)
import Text.Parsec.String (Parser, parseFromFile)

makePairs :: [a] -> [(a, a)]
makePairs [] = []
makePairs xs = (a, b) : makePairs (drop 2 xs) where [a, b] = take 2 xs

part1 :: IO ()
part1 = do
  input' <- parseFromFile parser "input.txt" >>= either (error . show) return
  let input = makePairs input'
  -- print `traverse` input
  -- print $ uncurry compare <$> input
  print $ sum $ (+ 1) <$> elemIndices LT (uncurry compare <$> input)
  return ()

dividers :: [Packet]
dividers =
  [ ListPacket [ListPacket [IntPacket 2]],
    ListPacket [ListPacket [IntPacket 6]]
  ]

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  -- print `traverse` sort (input ++ dividers)
  let sorted = sort (input ++ dividers)
  print $ product $ (+ 1) <$> findIndices (`elem` dividers) sorted
  return ()

data Packet = IntPacket Int | ListPacket [Packet] deriving (Show, Eq)

instance Ord Packet where
  IntPacket left `compare` IntPacket right = left `compare` right
  left@(IntPacket _) `compare` right@(ListPacket _) = ListPacket [left] `compare` right
  left@(ListPacket _) `compare` right@(IntPacket _) = left `compare` ListPacket [right]
  ListPacket [] `compare` ListPacket [] = EQ
  ListPacket [] `compare` ListPacket _ = LT
  ListPacket _ `compare` ListPacket [] = GT
  ListPacket (l : ls) `compare` ListPacket (r : rs) = if l == r then ListPacket ls `compare` ListPacket rs else l `compare` r

parser :: Parser [Packet]
parser = packet `sepEndBy` many1 newline <* eof

packet :: Parser Packet
packet = listPacket <|> intPacket

listPacket :: Parser Packet
listPacket = ListPacket <$> between (char '[') (char ']') (packet `sepEndBy` char ',')

intPacket :: Parser Packet
intPacket = IntPacket <$> int

int :: Parser Int
int = read <$> many1 digit

main :: IO ()
main = do
  arg <- head . (++ ["all"]) <$> getArgs
  when (arg == "all" || arg == "part1") part1
  when (arg == "all" || arg == "part2") part2
