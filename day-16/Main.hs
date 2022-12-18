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
import Data.Maybe (catMaybes)
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
  print $ score input (nonZeroValves input) 30
  return ()

score :: Volcano -> Set ValveId -> Int -> Pressure
score volcano toVisit remainingTime = pressure $ maximumBy (comparing pressure) stepper
  where
    paths = generatePaths volcano
    stepper = step volcano paths toVisit $ State "AA" Set.empty 0 remainingTime

nonZeroValves :: Volcano -> Set ValveId
nonZeroValves volcano = Map.keysSet (Map.filter nonZero volcano) where nonZero = (> 0) . flowRate

generatePaths :: Volcano -> Map (ValveId, ValveId) [ValveId]
generatePaths volcano =
  Map.fromList $
    catMaybes
      [ ((from, to),) <$> path
        | let valves = Set.toList $ nonZeroValves volcano,
          from <- "AA" : valves,
          to <- valves,
          from /= to,
          let path = aStar volcano from to
      ]

step :: Volcano -> Map (ValveId, ValveId) [ValveId] -> Set ValveId -> State -> [State]
step volcano paths valvesToVisit state = go state
  where
    go state@State {current, open, pressure, remainingTime}
      | remainingTime <= 0 = return state
      | Set.notMember current open && Set.member current valvesToVisit = processTime state {open = Set.insert current open} 1
      | null currentPaths = processTime state remainingTime
      | otherwise = concatMap (\path -> processTime state {current = head path} (length path)) currentPaths
      where
        currentPaths = filter ((<= remainingTime) . length) $ Map.elems $ Map.restrictKeys paths (Set.fromList $ (current,) <$> Set.toList (valvesToVisit `Set.difference` open))
        processTime state time =
          go
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
  -- print input
  let valves = nonZeroValves input
  let (valve, valves') = Set.deleteFindMin valves
  let powerset = Set.insert valve <$> Set.toList (Set.powerSet valves')
  let valvesPairs = zip (Set.difference valves <$> powerset) powerset
  print $ maximum $ (\(human, elephant) -> score input human 26 + score input elephant 26) <$> valvesPairs
  return ()

type ValveId = String

data Valve = Valve {exits :: [ValveId], flowRate :: Pressure} deriving (Show)

type Pressure = Int

type Volcano = Map ValveId Valve

data State = State
  { current :: ValveId,
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
