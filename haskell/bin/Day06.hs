import AdventOfCode.Utils
import Data.List.Split (splitOn)

parseInput :: String -> [Integer]
parseInput = fmap strToInt . splitOn ","

calc :: Integer -> Integer
calc 0 = 6
calc n = n - 1

part1 :: String -> Integer
part1 = go 0 . parseInput
  where
    go :: Integer -> [Integer] -> Integer
    go 80 nums = fromIntegral $ length nums
    go d nums = go (d + 1) (next ++ replicate zeros 8)
      where
        next = calc <$> nums
        zeros = length $ filter (== 0) nums

main :: IO ()
main = do
  input <- readInput 6
  print (part1 input)
