import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Foldable (foldl')
import Data.Heap (Entry (Entry))
import qualified Data.Heap as Heap
import Data.List (maximumBy)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Strict ((!), (!?))
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs)
import Text.Parsec (count, eof, letter, newline, sepBy, string, try)
import Text.Parsec.Combinator (sepEndBy)
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number (int)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  -- print input
  -- print $ aStar input "AA" "QL"
  print $ maximumBy (comparing pressure) $ step $ State input "AA" Set.empty 0 30
  return ()

step :: State -> [State]
step state@State {volcano, current, open, pressure, remainingTime}
  | remainingTime <= 0 = [state]
  | nonZeroValves == open = processTime state remainingTime
  | Set.notMember current open && nonZero valve = processTime state {open = Set.insert current open} 1
  | otherwise = concatMap (\path -> processTime state {current = head path} (length path)) (filter ((< remainingTime) . length) paths)
  where
    valve = volcano ! current
    nonZero = (> 0) . flowRate
    nonZeroValves = Map.keysSet (Map.filter nonZero volcano)
    current' = current
    open' = Set.insert current open
    paths = mapMaybe (aStar volcano current) (Set.toList (nonZeroValves `Set.difference` open))
    processTime state time =
      step
        state
          { pressure = pressure + time * sum (flowRate <$> Map.elems (Map.restrictKeys volcano open)),
            remainingTime = remainingTime - time
          }

reconstructPath :: Map ValveId ValveId -> ValveId -> [ValveId]
reconstructPath cameFrom current =
  case cameFrom !? current of
    Nothing -> []
    Just previous -> current : reconstructPath cameFrom previous

aStar :: Volcano -> ValveId -> ValveId -> Maybe [ValveId]
aStar graph from to = go openSet cameFrom gScore
  where
    openSet = Heap.singleton (Entry 1 from)
    cameFrom = Map.empty
    gScore = Map.singleton from 0
    go openSet cameFrom gScore = case Heap.uncons openSet of
      Nothing -> Nothing
      Just (Entry _ current, openSet') ->
        if current == to
          then Just $ reconstructPath cameFrom current
          else go openSet'' cameFrom' gScore'
        where
          (openSet'', cameFrom', gScore') = foldl' f (openSet', cameFrom, gScore) (exits $ graph ! current)
          f (o, c, g) n =
            let tentativeGScore = (case gScore !? current of Just n -> n; Nothing -> error "here") + 1
             in if case gScore !? n of Nothing -> True; Just x -> tentativeGScore < x
                  then
                    ( Heap.insert (Entry (tentativeGScore + 1) n) o,
                      Map.insert n current c,
                      Map.insert n tentativeGScore g
                    )
                  else (o, c, g)

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  return ()

type ValveId = String

data Valve = Valve {exits :: [ValveId], flowRate :: Pressure} deriving (Show)

type Pressure = Int

type Volcano = Map ValveId Valve

data State = State
  { volcano :: Volcano,
    current :: ValveId,
    open :: Set ValveId,
    pressure :: Pressure,
    remainingTime :: Int
  }
  deriving (Show)

parser :: Parser Volcano
parser = Map.fromList <$> valve `sepEndBy` newline <* eof

valve :: Parser (ValveId, Valve)
valve = do
  string "Valve "
  id <- valveId
  string " has flow rate="
  flowRate <- int
  exits <- tunnels
  return (id, Valve exits flowRate)

tunnels :: Parser [ValveId]
tunnels = try single <|> multiple
  where
    single = string "; tunnel leads to valve " *> count 1 valveId
    multiple = string "; tunnels lead to valves " *> valveId `sepBy` string ", "

valveId :: Parser ValveId
valveId = count 2 letter

main :: IO ()
main = do
  arg <- head . (++ ["all"]) <$> getArgs
  when (arg == "all" || arg == "part1") part1
  when (arg == "all" || arg == "part2") part2
