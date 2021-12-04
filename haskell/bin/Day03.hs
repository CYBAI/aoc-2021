{-# LANGUAGE NamedFieldPuns #-}

import AdventOfCode.Utils
import Data.Vector (Vector)
import Data.Vector as V (foldl', fromList, head, map, replicate, zip)

type Consumption = (Integer, Integer)

increment :: Consumption -> Char -> Consumption
increment (z, o) '0' = (z + 1, o)
increment (z, o) '1' = (z, o + 1)
increment _ _ = error "Unexpected character"

part1 :: String -> Integer
part1 input = gamma * epsilon
  where
    (gamma, epsilon, _) = V.foldl' go (0, 0, toInteger (binLength - 1)) (V.foldl' transform initial digits)

    go :: (Integer, Integer, Integer) -> Consumption -> (Integer, Integer, Integer)
    go (accX, accY, len) (x, y) = if x > y then (accX + 2 ^ len, accY, len - 1) else (accX, accY + 2 ^ len, len - 1)

    digits :: Vector String
    digits = V.fromList $ lines input

    binLength = length $ V.head digits

    initial :: Vector Consumption
    initial = V.replicate binLength (0, 0)

    transform :: Vector Consumption -> String -> Vector Consumption
    transform consumption digit = V.map (uncurry increment) $ V.zip consumption (V.fromList digit)

main :: IO ()
main = do
  input <- readInput 3
  putStrLn $ show (part1 input)
