{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

import AdventOfCode.Class
import AdventOfCode.Utils
import Data.List (stripPrefix)

data Direction = Forward | Down | Up

data Command = Command
  { direction :: Direction,
    distance :: Integer
  }

parse :: String -> Command
parse str | Just num <- "forward " `stripPrefix` str = Command {direction = Forward, distance = read @Integer num}
parse str | Just num <- "down " `stripPrefix` str = Command {direction = Down, distance = read @Integer num}
parse str | Just num <- "up " `stripPrefix` str = Command {direction = Up, distance = read @Integer num}
parse _ = error "Unexpected commands"

newtype Day02 = Day02 Integer
  deriving (Show)

instance Solutions Day02 where
  part1 input = Day02 (horizontal * depth)
    where
      (horizontal, depth) = foldl calc (0, 0) $ parse <$> lines input

      calc :: (Integer, Integer) -> Command -> (Integer, Integer)
      calc (x, y) Command {direction = Forward, distance} = (x + distance, y)
      calc (x, y) Command {direction = Down, distance} = (x, y + distance)
      calc (x, y) Command {direction = Up, distance} = (x, y - distance)

  part2 input = Day02 (horizontal * depth)
    where
      (horizontal, depth, _) = foldl calc (0, 0, 0) $ parse <$> lines input

      calc :: (Integer, Integer, Integer) -> Command -> (Integer, Integer, Integer)
      calc (x, y, aim) Command {direction = Down, distance} = (x, y, aim + distance)
      calc (x, y, aim) Command {direction = Up, distance} = (x, y, aim - distance)
      calc (x, y, aim) Command {direction = Forward, distance} = (x + distance, y + aim * distance, aim)

main :: IO ()
main = do
  input <- readInput 2
  putStrLn $ show (part1 @Day02 input)
  putStrLn $ show (part2 @Day02 input)
