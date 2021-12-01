{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

import AdventOfCode.Class
import AdventOfCode.Utils

newtype Day01 = Day01 Integer
  deriving (Show)

instance Solutions Day01 where
  part1 input = Day01 $ go 0 depths
    where
      depths = (\n -> read n :: Integer) <$> lines input

      go :: Integer -> [Integer] -> Integer
      go count (x : y : []) = increment x y $ count
      go count (x : y : xs) = go (increment x y $ count) (y : xs)
      go count _ = count

      increment :: Integer -> Integer -> (Integer -> Integer)
      increment x y = if x < y then (+ 1) else (+ 0)

  part2 = undefined

main :: IO ()
main = do
  input <- readInput 1
  putStrLn $ show (part1 @Day01 input)
