import Control.Monad (when)
import Data.Foldable (Foldable (foldl'))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Debug.Trace (traceShow, traceShowId)
import System.Environment (getArgs)
import Text.Parsec (eof)
import Text.Parsec.Char (newline)
import Text.Parsec.Combinator (sepEndBy)
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number (int)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  -- print input
  let ids = Seq.fromList [0 .. length input - 1]
  let result = Seq.index input <$> foldl' shift ids (Seq.zip ids input)
  -- print result
  let Just zeroIndex = Seq.elemIndexL 0 result
  let indices = (result `Seq.index`) . (`mod` Seq.length input) <$> [zeroIndex + 1000, zeroIndex + 2000, zeroIndex + 3000]
  print $ sum indices
  return ()

shift :: Seq Int -> (Int, Int) -> Seq Int
shift seq (id, amount) = Seq.insertAt (amount' `mod` Seq.length seq') id seq'
  where
    Just index = Seq.elemIndexL id seq
    seq' = Seq.deleteAt index seq
    amount' = index + amount

decryptionKey :: Int
decryptionKey = 811589153

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let input' = (* decryptionKey) <$> input
  -- print input
  let ids = Seq.fromList [0 .. length input' - 1]
  let result = Seq.index input' <$> foldl' shift ids (Seq.cycleTaking (Seq.length input' * 10) (Seq.zip ids input'))
  -- print result
  let Just zeroIndex = Seq.elemIndexL 0 result
  let indices = (result `Seq.index`) . (`mod` Seq.length input') <$> [zeroIndex + 1000, zeroIndex + 2000, zeroIndex + 3000]
  print $ sum indices
  return ()

parser :: Parser (Seq Int)
parser = Seq.fromList <$> int `sepEndBy` newline <* eof

main :: IO ()
main = do
  arg <- head . (++ ["all"]) <$> getArgs
  when (arg == "all" || arg == "part1") part1
  when (arg == "all" || arg == "part2") part2
