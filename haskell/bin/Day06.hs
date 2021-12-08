import AdventOfCode.Utils
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

parseInput :: String -> [Integer]
parseInput = fmap strToInt . splitOn ","

next :: Integer -> Integer -> [Integer] -> [Integer]
next _ _ [] = error "Unexpected empty array"
next 0 c (x : xs) = x + c : xs
next n c (x : xs) = x : next (n - 1) c xs

solve :: Integer -> [Integer] -> Integer
solve 0 xs = sum xs
solve n xs = solve (n - 1) (go xs)
  where
    go :: [Integer] -> [Integer]
    go [] = error "Unexpected empty array"
    go (x : xs) = next 6 x xs ++ [x]

main :: IO ()
main = do
  input <- readInput 6
  let nums = genNums input
  print (solve 80 nums)
  print (solve 256 nums)
  where
    genNums = M.elems . foldr (M.alter ((+ 1) <$>)) (M.fromList $ zip [0 .. 8] (replicate 9 0)) . parseInput
