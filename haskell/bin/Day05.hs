import AdventOfCode.Utils
import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

data Coord = X Integer | Y Integer

type Point = (Integer, Integer)

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

part1 :: String -> Int
part1 = M.size . M.filter (>= 2) . foldl' f M.empty . parseInput . lines
  where
    f :: M.Map Point Integer -> (Point, Point) -> M.Map Point Integer
    f hm ((x1, y1), (x2, y2)) | x1 == x2 = foldl' (g (X x1)) hm (range $ order y1 y2)
    f hm ((x1, y1), (x2, y2)) | y1 == y2 = foldl' (g (Y y1)) hm (range $ order x1 x2)
    f hm _ = hm

    g :: Coord -> M.Map Point Integer -> Integer -> M.Map Point Integer
    g coord hm n =
      case coord of
        (X x) -> M.alter up' (x, n) hm
        (Y y) -> M.alter up' (n, y) hm
      where
        up' :: Maybe Integer -> Maybe Integer
        up' Nothing = Just 1
        up' (Just x) = Just (x + 1)

    order :: Integer -> Integer -> (Integer, Integer)
    order x y = if x < y then (x, y) else (y, x)

    range :: (Integer, Integer) -> [Integer]
    range (x, y) = [x .. y]

main :: IO ()
main = do
  input <- readInput 5
  print (part1 input)
