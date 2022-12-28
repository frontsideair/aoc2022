import Control.Monad (when)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs)
import Text.Parsec (newline, sepEndBy, string)
import Text.Parsec.Combinator (eof)
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number (int)
import Prelude hiding (id)

timeLimit, timeLimit' :: Int
timeLimit = 24
timeLimit' = 32

blueprintLimit :: Int
blueprintLimit = 3

initialState :: State
initialState = (Resources 0 0 0 0, Robots 1 0 0 0)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  -- print input
  -- let states = iterate (\l -> let xs = concatMap (step (head input)) l; geodeCount = maximum $ geode . fst <$> xs in filter (\(resources, _) -> geode resources == geodeCount) xs) [initialState] !! timeLimit
  -- print $ length states
  -- print $ maximum $ geode . fst <$> states
  print $ sum $ qualityLevel timeLimit <$> input
  return ()

maxGeodes :: Int -> BluePrint -> Int
maxGeodes timeLimit blueprint = maximum $ geode . fst <$> Set.toList states
  where
    states = step blueprint 0 timeLimit (Set.singleton initialState)

qualityLevel :: Int -> BluePrint -> Int
qualityLevel timeLimit blueprint@BluePrint {id} = maxGeodes timeLimit blueprint * id

step :: BluePrint -> Int -> Int -> Set State -> Set State
step blueprint maxGeodesSoFar remainingTime states =
  if remainingTime == 1
    then Set.map (\state -> collectResources state state) states
    else
      step
        blueprint
        maxGeodesSoFar'
        (remainingTime - 1)
        (Set.fromList $ filter (\state -> futurePotential (remainingTime - 1) state >= maxGeodesSoFar) states')
  where
    actions state = take 3 $ mapMaybe (buildRobot remainingTime state blueprint) [GeodeRobot, ObsidianRobot, ClayRobot, OreRobot] ++ [state]
    collectResources (_, Robots {..}) (Resources {..}, robots) =
      (Resources (ore + oreRobotCount) (clay + clayRobotCount) (obsidian + obsidianRobotCount) (geode + geodeRobotCount), robots)
    neighbors state = collectResources state <$> actions state
    states' = concatMap neighbors states
    maxGeodesSoFar' = maximum $ geode . fst <$> states'

futurePotential :: Int -> State -> Int
futurePotential remainingTime (Resources {geode}, Robots {geodeRobotCount}) =
  geode + sum [geodeRobotCount .. geodeRobotCount + remainingTime] - 1

buildRobot :: Int -> State -> BluePrint -> Robot -> Maybe State
buildRobot remainingTime (resources, Robots {..}) BluePrint {..} robot =
  if ore' >= 0 && clay' >= 0 && obsidian' >= 0 && isNeeded
    then
      Just
        ( resources {ore = ore', clay = clay', obsidian = obsidian'},
          Robots oreRobotCount' clayRobotCount' obsidianRobotCount' geodeRobotCount'
        )
    else Nothing
  where
    cost = case robot of
      OreRobot -> oreRobot
      ClayRobot -> clayRobot
      ObsidianRobot -> obsidianRobot
      GeodeRobot -> geodeRobot
    remainingTime' = remainingTime - 1
    minTime = min remainingTime'
    obsidianRobotRequired = minTime $ ((obsidian geodeRobot * remainingTime' - obsidian resources) `div'` remainingTime') - obsidianRobotCount
    clayRobotRequired = minTime $ ((clay obsidianRobot * remainingTime' - clay resources) `div'` remainingTime') - clayRobotCount
    oreRobotRequired = minTime $ ((maximum ((* remainingTime') <$> [ore clayRobot, ore obsidianRobot, ore geodeRobot]) - ore resources) `div'` remainingTime') - oreRobotCount
    isNeeded = case robot of
      OreRobot -> geodeRobotCount == 0 && obsidianRobotCount == 0 && oreRobotRequired > 0
      ClayRobot -> geodeRobotCount == 0 && clayRobotRequired > 0
      ObsidianRobot -> obsidianRobotRequired > 0
      GeodeRobot -> True
    ore' = ore resources - ore cost
    clay' = clay resources - clay cost
    obsidian' = obsidian resources - obsidian cost
    oreRobotCount' = oreRobotCount + if robot == OreRobot then 1 else 0
    clayRobotCount' = clayRobotCount + if robot == ClayRobot then 1 else 0
    obsidianRobotCount' = obsidianRobotCount + if robot == ObsidianRobot then 1 else 0
    geodeRobotCount' = geodeRobotCount + if robot == GeodeRobot then 1 else 0

div' :: Int -> Int -> Int
div' a b = if a `mod` b == 0 then a `div` b else a `div` b + 1

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  print $ product $ maxGeodes timeLimit' <$> take blueprintLimit input
  return ()

data Resources = Resources {ore :: Int, clay :: Int, obsidian :: Int, geode :: Int} deriving (Show, Eq, Ord)

data Robots = Robots {oreRobotCount :: Int, clayRobotCount :: Int, obsidianRobotCount :: Int, geodeRobotCount :: Int} deriving (Show, Eq, Ord)

data Robot = OreRobot | ClayRobot | ObsidianRobot | GeodeRobot deriving (Show, Eq)

type State = (Resources, Robots)

data BluePrint = BluePrint {id :: Int, oreRobot :: Resources, clayRobot :: Resources, obsidianRobot :: Resources, geodeRobot :: Resources} deriving (Show)

parser :: Parser [BluePrint]
parser = blueprint `sepEndBy` newline <* eof

blueprint :: Parser BluePrint
blueprint = do
  string "Blueprint "
  id <- int
  string ": Each ore robot costs "
  oreRobotOreCost <- int
  string " ore. Each clay robot costs "
  clayRobotOreCost <- int
  string " ore. Each obsidian robot costs "
  obsidianRobotOreCost <- int
  string " ore and "
  obsidianRobotClayCost <- int
  string " clay. Each geode robot costs "
  geodeRobotOreCost <- int
  string " ore and "
  geodeRobotObsidianCost <- int
  string " obsidian."
  return $ BluePrint id (Resources oreRobotOreCost 0 0 0) (Resources clayRobotOreCost 0 0 0) (Resources obsidianRobotOreCost obsidianRobotClayCost 0 0) (Resources geodeRobotOreCost 0 geodeRobotObsidianCost 0)

main :: IO ()
main = do
  arg <- head . (++ ["all"]) <$> getArgs
  when (arg == "all" || arg == "part1") part1
  when (arg == "all" || arg == "part2") part2
