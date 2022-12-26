import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Functor (($>))
import qualified Data.Monoid as Monoid
import System.Environment (getArgs)
import Text.Parsec (char, eof, many1)
import Text.Parsec.Char (newline)
import Text.Parsec.Combinator (sepEndBy)
import Text.Parsec.String (Parser, parseFromFile)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let decimalSum = sum $ snafuToDecimal <$> input
  -- print decimalSum
  let snafuSum = decimalToSnafu decimalSum
  putStrLn $ concatMap show snafuSum
  -- print $ snafuToDecimal snafuSum
  return ()

snafuDigitToDecimal :: SnafuDigit -> Integer
snafuDigitToDecimal Two = 2
snafuDigitToDecimal One = 1
snafuDigitToDecimal Zero = 0
snafuDigitToDecimal Minus = -1
snafuDigitToDecimal DoubleMinus = -2

snafuToDecimal :: SnafuNumber -> Integer
snafuToDecimal snafuNumber = sum $ zipWith f (snafuDigitToDecimal <$> reverse snafuNumber) (iterate (* 5) 1)
  where
    f = (*)

decimalToSnafuDigit :: Integer -> SnafuDigit
decimalToSnafuDigit 2 = Two
decimalToSnafuDigit 1 = One
decimalToSnafuDigit 0 = Zero
decimalToSnafuDigit (-1) = Minus
decimalToSnafuDigit (-2) = DoubleMinus
decimalToSnafuDigit n = error $ "not a snafu digit: " ++ show n

decimalToSnafu :: Integer -> SnafuNumber
decimalToSnafu n = go mostSignificant n
  where
    mostSignificant = ceiling $ logBase 5 (fromIntegral (abs n) * 2)
    go m n = if m == 0 then [] else decimalToSnafuDigit digit : go (m - 1) rest
      where
        digit = signum n * if fromIntegral (abs n) > (5 ** (fromIntegral m - 1)) * 1.5 then 2 else if fromIntegral (abs n) < (5 ** (fromIntegral m - 2)) * 2 then 0 else 1
        rest = n - digit * floor (5 ** (fromIntegral m - 1))

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  return ()

data SnafuDigit = Two | One | Zero | Minus | DoubleMinus deriving (Eq, Ord)

instance Show SnafuDigit where
  show Two = "2"
  show One = "1"
  show Zero = "0"
  show Minus = "-"
  show DoubleMinus = "="

type SnafuNumber = [SnafuDigit]

parser :: Parser [SnafuNumber]
parser = snafuNumber `sepEndBy` newline <* eof

snafuNumber :: Parser SnafuNumber
snafuNumber = many1 snafuDigit

snafuDigit :: Parser SnafuDigit
snafuDigit =
  char '2' $> Two
    <|> char '1' $> One
    <|> char '0' $> Zero
    <|> char '-' $> Minus
    <|> char '=' $> DoubleMinus

main :: IO ()
main = do
  arg <- head . (++ ["all"]) <$> getArgs
  when (arg == "all" || arg == "part1") part1
  when (arg == "all" || arg == "part2") part2
