import AdventOfCode.Utils

depths :: String -> [Integer]
depths x = strToInt <$> lines x

increment :: Bool -> (Integer -> Integer)
increment bool = if bool then (+ 1) else (+ 0)

part1 :: String -> Integer
part1 input = go 0 (depths input)
  where
    go :: Integer -> [Integer] -> Integer
    go count (x : xs@(y : _)) = go (increment (x < y) count) xs
    go count _ = count

part2 :: String -> Integer
part2 input = go 0 (depths input)
  where
    go :: Integer -> [Integer] -> Integer
    go count (x : xs@(_ : _ : z : _)) = go (increment (x < z) count) xs
    go count _ = count

main :: IO ()
main = do
  input <- readInput 1
  print (part1 input)
  print (part2 input)
