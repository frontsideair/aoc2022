import Data.Char (isLower, ord)
import Data.List (foldl1')
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec (letter, sepEndBy)
import Text.Parsec.Char (newline)
import Text.Parsec.Combinator (many1)
import Text.Parsec.String (Parser, parseFromFile)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let rucksacks = compartmentalize <$> input
  let shared = Set.findMin . uncurry Set.intersection <$> rucksacks
  print $ sum $ priority <$> shared

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let elves = chunksOf 3 $ Set.fromList <$> input
  let badges = Set.findMin . foldl1' Set.intersection <$> elves
  print $ sum $ priority <$> badges

compartmentalize :: Ord a => [a] -> (Set a, Set a)
compartmentalize rucksack =
  let (left, right) = splitAt (length rucksack `div` 2) rucksack
   in (Set.fromList left, Set.fromList right)

priority :: Char -> Int
priority char =
  if isLower char
    then ord char - ord 'a' + 1
    else ord char - ord 'A' + 27

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = let (l, r) = splitAt n xs in l : chunksOf n r

type Compartment = Set Char

type Rucksack = [Char]

parser :: Parser [Rucksack]
parser = rucksack `sepEndBy` newline

rucksack :: Parser Rucksack
rucksack = many1 letter

main :: IO ()
main = do
  part1
  part2