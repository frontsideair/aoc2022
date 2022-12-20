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

solve input = do
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
  return $ xsCount + yxCount + zsCount

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  solve input >>= print
  return ()

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let xs = Set.map (\(x, y, z) -> x) input
  let ys = Set.map (\(x, y, z) -> y) input
  let zs = Set.map (\(x, y, z) -> z) input
  -- print (xs, ys, zs)
  let withinBounds (x, y, z) = within (minimum xs, maximum xs) x && within (minimum ys, maximum ys) y && within (minimum zs, maximum zs) z
  let space = Set.fromList [(x, y, z) | x <- [minimum xs .. maximum xs], y <- [minimum ys .. maximum ys], z <- [minimum zs .. maximum zs]]
  -- print $ neighbors withinBounds (1, 1, 1)
  let emptySpace = walkSpace (\cube -> withinBounds cube && Set.notMember cube input) Set.empty (Set.singleton (minimum xs, minimum ys, minimum zs))
  let fullSpace = space `Set.difference` emptySpace
  -- print $ Set.size fullSpace
  -- print $ Set.size input
  solve fullSpace >>= print
  return ()

walkSpace :: (Cube -> Bool) -> Set Cube -> Set Cube -> Set Cube
walkSpace withinBounds acc toVisit = case Set.minView toVisit of
  Nothing -> acc
  Just (x, xs) -> walkSpace withinBounds acc' (Set.difference (Set.fromList (neighbors withinBounds x) `Set.union` xs) acc')
    where
      acc' = Set.insert x acc

within :: (Int, Int) -> Int -> Bool
within (min, max) x = x >= min && x <= max

-- only up, down, left, right, front, back; no diagonals
neighbors :: (Cube -> Bool) -> Cube -> [Cube]
neighbors withinBounds (x, y, z) = [(x + dx, y + dy, z + dz) | dx <- [-1 .. 1], dy <- [-1 .. 1], dz <- [-1 .. 1], (abs dx + abs dy + abs dz) == 1, withinBounds (x + dx, y + dy, z + dz)]

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
