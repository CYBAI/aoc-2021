import AdventOfCode.Utils
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

parseInput :: String -> [Integer]
parseInput = fmap strToInt . splitOn ","

collect :: [Integer] -> M.Map Integer Integer
collect = go M.empty
  where
    go hm [] = hm
    go hm (x : xs) = go (M.alter inc x hm) xs

    inc :: Maybe Integer -> Maybe Integer
    inc Nothing = Just 1
    inc (Just x) = Just (x + 1)

fuel :: (Integer -> Integer -> Integer -> Integer) -> M.Map Integer Integer -> Integer -> Integer
fuel op hm n = M.foldlWithKey go 0 hm
  where
    go :: Integer -> Integer -> Integer -> Integer
    go acc k _ | n == k = acc
    go acc k c = acc + op n k c

calculate :: (Integer -> Integer -> Integer -> Integer) -> M.Map Integer Integer -> [Integer]
calculate op hm = fuel op hm <$> [minB .. maxB]
  where
    minB = case M.lookupMin hm of
      Just (x, _) -> x
      Nothing -> error "Unexpected no minimum value"
    maxB = case M.lookupMax hm of
      Just (x, _) -> x
      Nothing -> error "Unexpected no maximum value"

part1 :: [Integer] -> Integer
part1 = minimum . calculate solve . collect
  where
    solve n k c = abs (n - k) * c

part2 :: [Integer] -> Integer
part2 = minimum . calculate solve . collect
  where
    solve n k c = ((1 + r) * r `div` 2) * c
      where
        r = abs (n - k)

main :: IO ()
main = do
  input <- readInput 7
  let nums = parseInput input
  print (part1 nums)
  print (part2 nums)
