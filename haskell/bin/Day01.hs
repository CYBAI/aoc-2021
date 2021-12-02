{-# LANGUAGE NamedFieldPuns #-}

import AdventOfCode.Utils

depths :: String -> [Integer]
depths x = (\n -> read n :: Integer) <$> lines x

increment :: Integer -> Integer -> (Integer -> Integer)
increment x y = if x < y then (+ 1) else (+ 0)

part1 :: String -> Integer
part1 input = go 0 (depths input)
  where
    go :: Integer -> [Integer] -> Integer
    go count (x : y : []) = increment x y $ count
    go count (x : xs@(y : _)) = go (increment x y $ count) xs
    go count _ = count

part2 :: String -> Integer
part2 input = go 0 0 Nothing (depths input)
  where
    go :: Integer -> Integer -> Maybe Integer -> [Integer] -> Integer
    go count _ Nothing (_ : _ : _ : []) = count
    go count _ Nothing (x : xs@(y : z : _)) = go count (x + y + z) (Just x) xs
    go count prevSum (Just n) (_ : _ : z : []) = increment prevSum (prevSum - n + z) $ count
    go count prevSum (Just n) (x : xs@(_ : z : _)) = go (increment prevSum next $ count) next (Just x) xs
      where
        next = prevSum - n + z
    go count _ _ _ = count

main :: IO ()
main = do
  input <- readInput 1
  putStrLn $ show (part1 input)
  putStrLn $ show (part2 input)
