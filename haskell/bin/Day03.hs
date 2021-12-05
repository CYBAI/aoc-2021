{-# LANGUAGE NamedFieldPuns #-}

import AdventOfCode.Utils
import Data.Vector (Vector)
import qualified Data.Vector as V (cons, empty, foldl', fromList, head, length, map, null, replicate, tail, zip)

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

part2 :: String -> Integer
part2 input = (binaryToDecimal generator) * (binaryToDecimal scrubber)
  where
    -- To find `oxygen generator rating`, determine the `most common` value (0 or 1)
    -- in the current bit position, and keep only numbers with that bit in that position.
    -- If 0 and 1 are equally common, keep values with a 1 in the position being considered.
    generator = find 0 (>) digits

    -- To find `CO2 scrubber rating`, determine the `least common` value (0 or 1)
    -- in the current bit position, and keep only numbers with that bit in that position.
    -- If 0 and 1 are equally common, keep values with a 0 in the position being considered.
    scrubber = find 0 (<=) digits

    digits :: Vector String
    digits = V.fromList $ lines input

    separate :: Vector String -> Int -> (Vector String, Vector String)
    separate = go (V.empty, V.empty)
      where
        go :: (Vector String, Vector String) -> Vector String -> Int -> (Vector String, Vector String)
        go (zeros, ones) xs n
          | V.length xs > 0 =
              let (digit, rest) = (V.head xs, V.tail xs)
               in case digit !! n of
                    '0' -> go (V.cons digit zeros, ones) rest n
                    '1' -> go (zeros, V.cons digit ones) rest n
                    _ -> error "Unexpected digit"
        go (zeros, ones) _ _ = (zeros, ones)

    find :: Int -> (Int -> Int -> Bool) -> Vector String -> String
    find _ _ xs | V.null xs = error "Unexpected empty array"
    find _ _ xs | V.length xs == 1 = V.head xs
    find n predicate xs = find (n + 1) predicate next
      where
        (zeros, ones) = separate xs n
        next = if predicate (V.length zeros) (V.length ones) then zeros else ones

    binaryToDecimal :: String -> Integer
    binaryToDecimal xs = go xs (length xs - 1) 0
      where
        go ('0' : next) len s = go next (len - 1) s
        go ('1' : next) len s = go next (len - 1) (s + 2 ^ len)
        go _ _ s = s

main :: IO ()
main = do
  input <- readInput 3
  putStrLn $ show (part1 input)
  putStrLn $ show (part2 input)
