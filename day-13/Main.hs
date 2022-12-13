import Control.Applicative ((<|>))
import Control.Monad (when)
import System.Environment (getArgs)
import Text.Parsec (digit, many1, newline, sepEndBy)
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (eof)
import Text.Parsec.String (Parser, parseFromFile)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  -- print `traverse` input
  print $ uncurry compare <$> input
  print $ sum $ fst <$> filter ((== LT) . snd) (zip [1 ..] $ uncurry compare <$> input)
  return ()

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  return ()

data Packet = IntPacket Int | ListPacket [Packet] deriving (Show)

instance Eq Packet where
  IntPacket left == IntPacket right = left == right
  left@(IntPacket _) == right@(ListPacket _) = ListPacket [left] == right
  left@(ListPacket _) == right@(IntPacket _) = left == ListPacket [right]
  ListPacket [] == ListPacket [] = True
  ListPacket [] == ListPacket _ = False
  ListPacket _ == ListPacket [] = False
  ListPacket (l : ls) == ListPacket (r : rs) = l == r && ListPacket ls == ListPacket rs

instance Ord Packet where
  IntPacket left <= IntPacket right = left <= right
  left@(IntPacket _) <= right@(ListPacket _) = ListPacket [left] <= right
  left@(ListPacket _) <= right@(IntPacket _) = left <= ListPacket [right]
  ListPacket [] <= ListPacket _ = True
  ListPacket _ <= ListPacket [] = False
  ListPacket (l : ls) <= ListPacket (r : rs) = l < r || l == r && (ListPacket ls <= ListPacket rs)

parser :: Parser [(Packet, Packet)]
parser = pair `sepEndBy` newline <* eof

pair :: Parser (Packet, Packet)
pair = (,) <$> packet <* newline <*> packet <* newline

packet :: Parser Packet
packet = listPacket <|> intPacket

listPacket :: Parser Packet
listPacket = ListPacket <$> (char '[' *> packet `sepEndBy` char ',' <* char ']')

intPacket :: Parser Packet
intPacket = IntPacket <$> int

int :: Parser Int
int = read <$> many1 digit

main :: IO ()
main = do
  arg <- head . (++ ["all"]) <$> getArgs
  when (arg == "all" || arg == "part1") part1
  when (arg == "all" || arg == "part2") part2
