import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Foldable (find)
import Data.Functor (($>))
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs)
import Text.Parsec (eof, many1, newline)
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (sepEndBy)
import Text.Parsec.String (Parser, parseFromFile)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  -- print input
  -- print $ proposal 0 input <$> Set.toList input
  let (_, map, _) = iterate step (0, input, False) !! 10
  -- print map
  let ((minY, minX), (maxY, maxX)) = bounds map
  print $ length [(y, x) | y <- [minY .. maxY], x <- [minX .. maxX], (y, x) `Set.notMember` map]
  return ()

proposal :: Int -> Set Coord -> Coord -> Maybe (Coord, Coord) -- (to, from)
proposal turn map (y, x) = do
  coord' <- headMay $ catMaybes $ take 4 $ drop turn $ cycle [north, south, west, east]
  if allEmpty then Nothing else return (coord', (y, x))
  where
    allEmpty = Set.fromList [(y', x') | y' <- [y - 1 .. y + 1], x' <- [x - 1 .. x + 1], (y', x') /= (y, x)] `Set.disjoint` map
    north = if Set.fromList [(y - 1, x') | x' <- [x - 1 .. x + 1]] `Set.disjoint` map then Just (y - 1, x) else Nothing
    south = if Set.fromList [(y + 1, x') | x' <- [x - 1 .. x + 1]] `Set.disjoint` map then Just (y + 1, x) else Nothing
    west = if Set.fromList [(y', x - 1) | y' <- [y - 1 .. y + 1]] `Set.disjoint` map then Just (y, x - 1) else Nothing
    east = if Set.fromList [(y', x + 1) | y' <- [y - 1 .. y + 1]] `Set.disjoint` map then Just (y, x + 1) else Nothing

step :: (Int, Set Coord, Bool) -> (Int, Set Coord, Bool) -- done
step (turn, map, _) =
  let elfMovements = Set.fromList $ mapMaybe (proposal turn map) (Set.toList map)
      validMovements = Set.filter (\(to, _) -> Set.size (Set.filter (\(to', _) -> to == to') elfMovements) == 1) elfMovements
      elvesToMove = Set.map snd validMovements
      newElves = Set.map fst validMovements `Set.union` (map `Set.difference` elvesToMove)
   in (turn + 1, newElves, Set.null elvesToMove)

bounds :: Set Coord -> (Coord, Coord)
bounds map = ((minimum ys, minimum xs), (maximum ys, maximum xs))
  where
    (ys, xs) = unzip $ Set.toList map

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x : _) = Just x

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let Just (turn, _, _) = find (\(_, _, done) -> done) $ iterate step (0, input, False)
  print turn
  return ()

parser :: Parser (Set Coord)
parser = mapParser <* eof

mapParser :: Parser (Set Coord)
mapParser = Set.fromList <$> rows
  where
    row = fmap fst . filter snd . zip [1 ..] <$> many1 cell
    cell = char '.' $> False <|> char '#' $> True
    rows = concat . zipWith (\i row -> (i,) <$> row) [1 ..] <$> row `sepEndBy` newline

type Coord = (Int, Int)

main :: IO ()
main = do
  arg <- head . (++ ["all"]) <$> getArgs
  when (arg == "all" || arg == "part1") part1
  when (arg == "all" || arg == "part2") part2
