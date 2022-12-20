import Control.Monad (when)
import Data.List (group)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs)
import Text.Parsec (char, eof)
import Text.Parsec.Char (newline)
import Text.Parsec.Combinator (sepEndBy)
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number (int)

countTrueGroups :: [Bool] -> Int
countTrueGroups xs = length $ filter id $ head <$> group xs

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  -- print input
  let xs = Set.map (\(x, y, z) -> x) input
  let ys = Set.map (\(x, y, z) -> y) input
  let zs = Set.map (\(x, y, z) -> z) input
  -- print (xs, ys, zs)
  let xs' = (\(y, z) -> (,y,z) <$> [minimum xs .. maximum xs]) <$> ((,) <$> [minimum ys .. maximum ys] <*> [minimum zs .. maximum zs])
  let ys' = (\(x, z) -> (x,,z) <$> [minimum ys .. maximum ys]) <$> ((,) <$> [minimum xs .. maximum xs] <*> [minimum zs .. maximum zs])
  let zs' = (\(x, y) -> (x,y,) <$> [minimum zs .. maximum zs]) <$> ((,) <$> [minimum xs .. maximum xs] <*> [minimum ys .. maximum ys])
  -- print `traverse` xs'
  -- print $ fmap (`Set.member` input) <$> xs'
  -- print $ fmap (`Set.member` input) <$> ys'
  -- print $ fmap (`Set.member` input) <$> zs'
  let xsCount = (2 *) $ sum $ countTrueGroups . fmap (`Set.member` input) <$> xs'
  let yxCount = (2 *) $ sum $ countTrueGroups . fmap (`Set.member` input) <$> ys'
  let zsCount = (2 *) $ sum $ countTrueGroups . fmap (`Set.member` input) <$> zs'
  print $ xsCount + yxCount + zsCount
  return ()

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  return ()

type Cube = (Int, Int, Int)

type Droplet = Set Cube

parser :: Parser Droplet
parser = Set.fromList <$> cube `sepEndBy` newline <* eof

cube :: Parser Cube
cube = do
  x <- int
  char ','
  y <- int
  char ','
  z <- int
  return (x, y, z)

main :: IO ()
main = do
  arg <- head . (++ ["all"]) <$> getArgs
  when (arg == "all" || arg == "part1") part1
  when (arg == "all" || arg == "part2") part2
