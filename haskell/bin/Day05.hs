{-# LANGUAGE TupleSections #-}

import AdventOfCode.Utils
import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

type Point = (Integer, Integer)

slope :: Point -> Point -> Integer
slope (x1, y1) (x2, y2) = abs $ (x1 - x2) `div` (y1 - y2)

findDiagonalPoints :: Point -> Point -> [Point]
findDiagonalPoints (x1, y1) (x2, y2) = zip (f x1 x2) (f y1 y2)
  where
    f a b = if a > b then [a, a - 1 .. b] else [a .. b]

order :: Integer -> Integer -> (Integer, Integer)
order x y = if x < y then (x, y) else (y, x)

range :: (Integer, Integer) -> [Integer]
range (x, y) = [x .. y]

parsePoint :: String -> Point
parsePoint str = case strToInt <$> splitOn "," str of
  [p1, p2] -> (p1, p2)
  _ -> error "Unexpected input which doesn't exactly have 2 numbers in a point"

parseInput :: [String] -> [(Point, Point)]
parseInput = fmap (transform . splitOn " -> ")
  where
    transform :: [String] -> (Point, Point)
    transform [xp, yp] = (parsePoint xp, parsePoint yp)
    transform _ = error "Unexpected input which doesn't exactly have 2 points"

calculate :: M.Map Point Integer -> [Point] -> M.Map Point Integer
calculate = foldr (M.alter inc)
  where
    inc :: Maybe Integer -> Maybe Integer
    inc Nothing = Just 1
    inc (Just x) = Just (x + 1)

part1 :: String -> Int
part1 = M.size . M.filter (>= 2) . foldl' f M.empty . parseInput . lines
  where
    f :: M.Map Point Integer -> (Point, Point) -> M.Map Point Integer
    f hm ((x1, y1), (x2, y2)) | x1 == x2 = calculate hm (map (x1,) $ range $ order y1 y2)
    f hm ((x1, y1), (x2, y2)) | y1 == y2 = calculate hm (map (,y1) $ range $ order x1 x2)
    f hm _ = hm

part2 :: String -> Int
part2 = M.size . M.filter (>= 2) . foldl' f M.empty . parseInput . lines
  where
    f :: M.Map Point Integer -> (Point, Point) -> M.Map Point Integer
    f hm ((x1, y1), (x2, y2)) | x1 == x2 = calculate hm (map (x1,) $ range $ order y1 y2)
    f hm ((x1, y1), (x2, y2)) | y1 == y2 = calculate hm (map (,y1) $ range $ order x1 x2)
    f hm (p1, p2) | slope p1 p2 == 1 = calculate hm (findDiagonalPoints p1 p2)
    f hm _ = hm

main :: IO ()
main = do
  input <- readInput 5
  print (part1 input)
  print (part2 input)
