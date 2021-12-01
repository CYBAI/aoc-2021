module AdventOfCode.Class
  ( Solutions (..),
  )
where

class Show a => Solutions a where
  part1 :: String -> a
  part2 :: String -> a
