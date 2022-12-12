import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Char (ord)
import Data.Foldable (foldl')
import Data.Functor (($>))
import Data.Heap (Entry (Entry))
import qualified Data.Heap as Heap
import Data.List (sort)
import Data.Map (Map)
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import Data.Matrix (Matrix, (!))
import qualified Data.Matrix as Matrix
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs)
import Text.Parsec (char, letter, many1, sepEndBy)
import Text.Parsec.Char (newline)
import Text.Parsec.Combinator (eof)
import Text.Parsec.String (Parser, parseFromFile)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  -- print input
  let positions = Matrix.toList $ Matrix.mapPos (,) input
  let start = fst . head $ filter ((== startPos) . snd) positions
  let end = fst . head $ filter ((== endPos) . snd) positions
  print $ (length . fromJust) $ aStar start end input

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let positions = Matrix.toList $ Matrix.mapPos (,) input
  let starts = fst <$> filter ((<= letterToInt 'a') . snd) positions
  let end = fst . head $ filter ((== endPos) . snd) positions
  print $ minimum $ length <$> (\start -> aStar start end input) `mapMaybe` starts
  return ()

startPos, endPos :: Int
startPos = letterToInt 'a' - 1
endPos = letterToInt 'z' + 1

parser :: Parser (Matrix Int)
parser = Matrix.fromLists <$> row `sepEndBy` newline <* eof

row :: Parser [Int]
row = many1 cell

cell :: Parser Int
cell = char 'S' $> startPos <|> char 'E' $> endPos <|> letterToInt <$> letter

letterToInt :: Char -> Int
letterToInt c = ord c - ord 'a' + 1

type Coord = (Int, Int)

manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

reconstructPath :: Map Coord Coord -> Coord -> Set Coord
reconstructPath cameFrom current =
  case cameFrom !? current of
    Nothing -> Set.empty
    Just previous -> Set.insert current $ reconstructPath cameFrom previous

aStar :: Coord -> Coord -> Matrix Int -> Maybe (Set Coord)
aStar from to matrix = go openSet cameFrom gScore
  where
    openSet = Heap.singleton (Entry (manhattan from to) from)
    cameFrom = Map.empty
    gScore = Map.singleton from 0
    go openSet cameFrom gScore = case Heap.uncons openSet of
      Nothing -> Nothing -- failure
      Just (Entry _ current, openSet') ->
        if current == to
          then Just $ reconstructPath cameFrom current
          else go openSet'' cameFrom' gScore'
        where
          (openSet'', cameFrom', gScore') = foldl' f (openSet', cameFrom, gScore) (neighborSpaces matrix current)
          f (o, c, g) n =
            let tentativeGScore = (case gScore !? current of Just n -> n; Nothing -> error "here") + 1
             in if case gScore !? n of Nothing -> True; Just x -> tentativeGScore < x
                  then
                    ( Heap.insert (Entry (tentativeGScore + manhattan n to) n) o,
                      Map.insert n current c,
                      Map.insert n tentativeGScore g
                    )
                  else (o, c, g)

neighborSpaces :: Matrix Int -> Coord -> Set Coord
neighborSpaces matrix (x, y) =
  Set.fromList $
    mapMaybe
      ( \(x', y') -> do
          cell <- Matrix.safeGet x' y' matrix
          if isWalkable (matrix ! (x, y)) cell then Just (x', y') else Nothing
      )
      (neighborCoords (x, y))

isWalkable :: Int -> Int -> Bool
isWalkable current neighbor = current + 1 >= neighbor

neighborCoords :: Coord -> [Coord]
neighborCoords (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

main :: IO ()
main = do
  arg <- head . (++ ["all"]) <$> getArgs
  when (arg == "all" || arg == "part1") part1
  when (arg == "all" || arg == "part2") part2
