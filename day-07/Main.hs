{-# LANGUAGE NamedFieldPuns #-}

import Control.Applicative ((<|>))
import Data.Foldable (foldl')
import Data.Map (Map)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Text.Parsec (char, digit, letter, many1, sepEndBy, string)
import Text.Parsec.Char (newline)
import Text.Parsec.Combinator (eof)
import Text.Parsec.Prim (try)
import Text.Parsec.String (Parser, parseFromFile)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let (_, map) = foldl' step ([], Map.empty) input
  print $ sum $ filter (<= 100000) $ calculateSize map <$> Map.keys map
  return ()

type DirPath = [String]

data DirStat = DirStat
  { ownSize :: Int,
    children :: [DirPath]
  }
  deriving (Show)

step :: (DirPath, Map DirPath DirStat) -> Command -> (DirPath, Map DirPath DirStat)
step (cwd, map) command = case command of
  Cd Root -> ([], map)
  Cd Up -> (tail cwd, map)
  Cd (Relative path) -> (path : cwd, map)
  Ls lines -> (cwd, Map.insert cwd dirStat map)
    where
      dirStat = foldl' (step' cwd) (DirStat 0 []) lines

step' :: DirPath -> DirStat -> Line -> DirStat
step' cwd DirStat {ownSize, children} (File s _) = DirStat (ownSize + s) children
step' cwd DirStat {ownSize, children} (Dir name) = DirStat ownSize ((name : cwd) : children)

calculateSize :: Map DirPath DirStat -> DirPath -> Int
calculateSize map path = case Map.lookup path map of
  Nothing -> 0
  Just DirStat {ownSize, children} -> ownSize + sum (calculateSize map <$> children)

totalDisk, requiredDisk :: Int
totalDisk = 70000000
requiredDisk = 30000000

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let (_, map) = foldl' step ([], Map.empty) input
  let sizes = Map.mapWithKey (\dirPath _ -> calculateSize map dirPath) map
  let occupiedDisk = sizes ! []
  let toDelete = requiredDisk - (totalDisk - occupiedDisk)
  print $ minimum $ filter (>= toDelete) $ Map.elems sizes
  return ()

data Command = Cd Path | Ls [Line] deriving (Show)

data Path = Root | Up | Relative String deriving (Show)

data Line = File Int String | Dir String deriving (Show)

parser :: Parser [Command]
parser = many1 command <* eof

command :: Parser Command
command = Cd <$> cd <|> Ls <$> ls

cd :: Parser Path
cd = try $ do
  string "$ cd "
  ret <- Root <$ char '/' <|> Up <$ string ".." <|> Relative <$> dirName
  newline
  return ret

ls :: Parser [Line]
ls = do
  string "$ ls"
  newline
  line `sepEndBy` newline

line :: Parser Line
line = File <$> number <* char ' ' <*> fileName <|> Dir <$> (string "dir " *> dirName)

number :: Parser Int
number = read <$> many1 digit

fileName :: Parser String
fileName = many1 (letter <|> char '.')

dirName :: Parser String
dirName = many1 letter

main :: IO ()
main = do
  part1
  part2