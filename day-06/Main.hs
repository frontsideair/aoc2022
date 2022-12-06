import Data.Foldable (find)
import Data.List (tails)
import qualified Data.Set as Set
import Text.Parsec (anyChar, eof, letter, newline)
import Text.Parsec.Combinator (many1)
import Text.Parsec.String (Parser, parseFromFile)

aperture :: Int -> [a] -> [[a]]
aperture n xs = map (take n) (tails xs)

allDifferent :: Ord a => [a] -> Bool
allDifferent xs = length xs == length (Set.fromList xs)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let seqs = zip [4 ..] $ aperture 4 input
  print $ find (\(i, s) -> allDifferent s) seqs

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let seqs = zip [14 ..] $ aperture 14 input
  print $ find (\(i, s) -> allDifferent s) seqs

parser :: Parser String
parser = many1 letter <* newline <* eof

main :: IO ()
main = do
  part1
  part2