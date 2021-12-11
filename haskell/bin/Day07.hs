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

part1 :: [Integer] -> Integer
part1 xs = minimum $ map (\n -> M.foldlWithKey (calc n) 0 count) (M.keys count)
  where
    count = collect xs

    calc :: Integer -> Integer -> Integer -> Integer -> Integer
    calc n acc k _ | n == k = acc
    calc n acc k c = acc + (abs (n - k) * c)

main :: IO ()
main = do
  input <- readInput 7
  let nums = parseInput input
  print (part1 nums)
