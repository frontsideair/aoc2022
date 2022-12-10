import Control.Monad (when)
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Text.Parsec (count, digit, eof, many1)
import Text.Parsec.Char (newline)
import Text.Parsec.Combinator (sepEndBy)
import Text.Parsec.String (Parser, parseFromFile)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  -- print input
  let topLeft = Matrix.mapPos (\pos a -> get pos input topLeft) input
  -- print topLeft
  let reversed = reverseMatrix input
  let br = Matrix.mapPos (\pos a -> get pos reversed br) reversed
  let bottomRight = reverseMatrix br
  -- print bottomRight
  let visible = Matrix.mapPos (\(x, y) a -> let tl = Matrix.getElem x y topLeft; br = Matrix.getElem x y bottomRight in any (\b -> fst b < a) [vertical tl, horizontal tl, vertical br, horizontal br]) input
  -- print visible
  print $ length $ filter (== True) (Matrix.toList visible)
  return ()

reverseMatrix :: Matrix a -> Matrix a
reverseMatrix matrix = Matrix.fromList (Matrix.nrows matrix) (Matrix.ncols matrix) . reverse . Matrix.toList $ matrix

type Height' = (Int, Int)

data Height = Height {vertical :: Height', horizontal :: Height'} deriving (Show)

get :: (Int, Int) -> Matrix Int -> Matrix Height -> Height
get (x, y) m1 matrix = Height vertical' horizontal'
  where
    pv = (x - 1, y)
    ph = (x, y - 1)
    vertical' = maybe (-1, 0) (max' (fromMaybe (-1) $ uncurry Matrix.safeGet pv m1) . vertical) (Matrix.safeGet (x - 1) y matrix)
    horizontal' = maybe (-1, 0) (max' (fromMaybe (-1) $ uncurry Matrix.safeGet ph m1) . horizontal) (Matrix.safeGet x (y - 1) matrix)
    max' c (a, b) = if a > c then (a, b + 1) else (c, 1)

part2 :: IO ()
part2 = do
  return ()

scenicValue :: Height -> Height -> Int
scenicValue a b = snd (vertical a) * snd (horizontal a) * snd (vertical b) * snd (horizontal b)

parser :: Parser (Matrix Int)
parser = Matrix.fromLists <$> line `sepEndBy` newline <* eof

line :: Parser [Int]
line = many1 (read <$> count 1 digit)

main :: IO ()
main = do
  arg <- head . (++ ["all"]) <$> getArgs
  when (arg == "all" || arg == "part1") part1
  when (arg == "all" || arg == "part2") part2
