import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Foldable (find)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs)
import Text.Parsec (eof, manyTill, newline, oneOf, sepEndBy, try)
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (many1)
import Text.Parsec.String (Parser, parseFromFile)

invertMap :: Ord b => Map a [b] -> Map b [a]
invertMap = Map.fromListWith (++) . concatMap (\(a, bs) -> (,[a]) <$> bs) . Map.toList

start, target :: (Coord, Coord) -> Coord
start ((yMin, xMin), _) = (yMin - 1, xMin)
target (_, (yMax, xMax)) = (yMax + 1, xMax)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let b = bounds input
  let map = Map.filter (not . null) input
  let Just (_, _, steps) = solve b map 0 (start b) (target b)
  print steps
  return ()

step :: (Coord, Coord) -> Map Coord [Direction] -> Map Coord [Direction]
step bounds map = invertMap $ Map.mapWithKey (\direction coords -> move bounds direction <$> coords) (invertMap map)

neighbors :: (Coord, Coord) -> Map Coord [Direction] -> Coord -> Set Coord
neighbors bounds map (y, x) =
  Set.fromList
    [ coord'
      | coord' <- [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1), (y, x)],
        coord' `Map.notMember` map,
        within bounds coord'
    ]

within :: (Coord, Coord) -> Coord -> Bool
within b@((yMin, xMin), (yMax, xMax)) (y, x) = yMin <= y && y <= yMax && xMin <= x && x <= xMax || (y, x) == target b || (y, x) == start b

turn :: (Coord, Coord) -> (Map Coord [Direction], Set Coord, Int) -> (Map Coord [Direction], Set Coord, Int)
turn bounds (map, coords, n) = (map', coords', n + 1)
  where
    map' = step bounds map
    coords' = Set.unions $ Set.map (neighbors bounds map') coords

bounds :: Map Coord [Direction] -> (Coord, Coord)
bounds map = (topLeft, bottomRight)
  where
    topLeft = (minimum (fst <$> Map.keys map), minimum (snd <$> Map.keys map))
    bottomRight = (maximum (fst <$> Map.keys map), maximum (snd <$> Map.keys map))

solve :: (Coord, Coord) -> Map Coord [Direction] -> Int -> Coord -> Coord -> Maybe (Map Coord [Direction], Set Coord, Int)
solve bounds map steps start target = find (\(_, coords, _) -> Set.member target coords) (iterate (turn bounds) (map, Set.singleton start, steps))

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let b = bounds input
  let map = Map.filter (not . null) input
  let Just (map', _, steps) = solve b map 0 (start b) (target b)
  let Just (map'', _, steps') = solve b map' steps (target b) (start b)
  let Just (_, _, steps'') = solve b map'' steps' (start b) (target b)
  print steps''
  return ()

parser :: Parser (Map Coord [Direction])
parser = do
  manyTill (oneOf ".#") newline
  map <- mapParser
  manyTill (oneOf ".#") newline
  eof
  return map

mapParser :: Parser (Map Coord [Direction])
mapParser = Map.fromList . concat <$> rows
  where
    row = try $ char '#' *> (zip [2 ..] <$> many1 cell) <* char '#'
    cell = char '.' $> [] <|> char '>' $> [East] <|> char 'v' $> [South] <|> char '<' $> [West] <|> char '^' $> [North]
    rows = zipWith (\index row -> (\(index', cell) -> ((index, index'), cell)) <$> row) [2 ..] <$> (row `sepEndBy` newline)

type Coord = (Int, Int)

bound :: (Coord, Coord) -> Coord -> Coord
bound ((yMin, xMin), (yMax, xMax)) (y, x) = (f yMin yMax y, f xMin xMax x)
  where
    f lower upper n
      | n < lower = upper
      | n > upper = lower
      | otherwise = n

move :: (Coord, Coord) -> Direction -> Coord -> Coord
move bounds direction (y, x) = bound bounds (y', x')
  where
    (y', x') = case direction of
      North -> (y - 1, x)
      South -> (y + 1, x)
      East -> (y, x + 1)
      West -> (y, x - 1)

data Direction = North | South | East | West deriving (Eq, Show, Ord)

main :: IO ()
main = do
  arg <- head . (++ ["all"]) <$> getArgs
  when (arg == "all" || arg == "part1") part1
  when (arg == "all" || arg == "part2") part2
