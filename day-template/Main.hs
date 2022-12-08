import Control.Monad (when)
import System.Environment (getArgs)
import Text.Parsec.String (Parser, parseFromFile)

part1 :: IO ()
part1 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  return ()

part2 :: IO ()
part2 = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  return ()

main :: IO ()
main = do
  arg <- head . (++ ["all"]) <$> getArgs
  when (arg == "all" || arg == "part1") part1
  when (arg == "all" || arg == "part2") part2
