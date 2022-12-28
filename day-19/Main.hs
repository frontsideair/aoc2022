import Control.Monad (when)
import Data.Foldable (maximumBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import System.Environment (getArgs)
import Text.Parsec (newline, sepEndBy, string)
import Text.Parsec.Combinator (eof)
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number (int)
import Prelude hiding (id)

timeLimit :: Int
timeLimit = 24

initialState :: State
initialState = (Resources 0 0 0 0, Robots 1 0 0 0)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  -- print input
  -- let states = iterate (\l -> let xs = concatMap (step (head input)) l; geodeCount = maximum $ geode . fst <$> xs in filter (\(resources, _) -> geode resources == geodeCount) xs) [initialState] !! timeLimit
  -- print $ length states
  -- print $ maximum $ geode . fst <$> states
  print $ sum $ qualityLevel <$> input
  return ()

qualityLevel :: BluePrint -> Int
qualityLevel blueprint@BluePrint {id} =
  let states = iterate f [initialState] !! timeLimit
      f l =
        let xs = concatMap (step blueprint) l
            geodeCount = maximum $ geode . fst <$> xs
         in xs -- if geodeCount == 0 then xs else filter (\(resources, _) -> geode resources == geodeCount) xs
      geodeCount = maximum $ geode . fst <$> states
   in id * geodeCount

step :: BluePrint -> State -> [State]
step blueprint state@(resources, Robots {oreRobotCount, clayRobotCount, obsidianRobotCount, geodeRobotCount}) =
  collectResources <$> actions
  where
    maybeGeodeRobot : otherRobots = buildRobot state blueprint <$> [GeodeRobot, ObsidianRobot, ClayRobot, OreRobot]
    actions = case maybeGeodeRobot of
      Just geodeRobot -> [geodeRobot] -- if can build geode robot, do it
      Nothing -> take 3 $ catMaybes otherRobots ++ [state] -- otherwise if can build all 3, try them all, otherwise doing nothing is an option
    collectResources (Resources {ore, clay, obsidian, geode}, robots) =
      (Resources (ore + oreRobotCount) (clay + clayRobotCount) (obsidian + obsidianRobotCount) (geode + geodeRobotCount), robots)

buildRobot :: State -> BluePrint -> Robot -> Maybe State
buildRobot (resources, robots) BluePrint {oreRobot, clayRobot, obsidianRobot, geodeRobot} robot =
  if ore' >= 0 && clay' >= 0 && obsidian' >= 0 && not isNotNeeded
    then
      Just
        ( resources {ore = ore', clay = clay', obsidian = obsidian'},
          Robots
            { oreRobotCount = oreRobotCount',
              clayRobotCount = clayRobotCount',
              obsidianRobotCount = obsidianRobotCount',
              geodeRobotCount = geodeRobotCount'
            }
        )
    else Nothing
  where
    cost = case robot of
      OreRobot -> oreRobot
      ClayRobot -> clayRobot
      ObsidianRobot -> obsidianRobot
      GeodeRobot -> geodeRobot
    robotCosts = [oreRobot, clayRobot, obsidianRobot, geodeRobot]
    isNotNeeded = case robot of
      OreRobot -> oreRobotCount robots >= maximum (ore <$> robotCosts)
      ClayRobot -> clayRobotCount robots >= maximum (clay <$> robotCosts)
      ObsidianRobot -> obsidianRobotCount robots >= maximum (obsidian <$> robotCosts)
      GeodeRobot -> False
    ore' = ore resources - ore cost
    clay' = clay resources - clay cost
    obsidian' = obsidian resources - obsidian cost
    oreRobotCount' = oreRobotCount robots + if robot == OreRobot then 1 else 0
    clayRobotCount' = clayRobotCount robots + if robot == ClayRobot then 1 else 0
    obsidianRobotCount' = obsidianRobotCount robots + if robot == ObsidianRobot then 1 else 0
    geodeRobotCount' = geodeRobotCount robots + if robot == GeodeRobot then 1 else 0

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  return ()

data Resources = Resources {ore :: Int, clay :: Int, obsidian :: Int, geode :: Int} deriving (Show)

data Robots = Robots {oreRobotCount :: Int, clayRobotCount :: Int, obsidianRobotCount :: Int, geodeRobotCount :: Int} deriving (Show)

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
