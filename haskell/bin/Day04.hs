{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import AdventOfCode.Utils
import Data.List (foldl')
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map.Strict as M

type Coord = (Integer, Integer)

type Board = M.Map Integer Coord

strToInt :: String -> Integer
strToInt = read @Integer

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

parseBoard :: [String] -> Board
parseBoard = fst . foldl' toBoard (M.empty, 0) . map words
  where
    toBoard :: (Board, Integer) -> [String] -> (Board, Integer)
    toBoard (board, rn) = (,rn + 1) . fst3 . foldl' f (board, rn, 0) . map strToInt

    f :: (Board, Integer, Integer) -> Integer -> (Board, Integer, Integer)
    f (board, x, y) n = (M.insert n (x, y) board, x, y + 1)

-- integer of key means "the row/column number"
-- integer of value means "(counts of rows, sum)"
type BingoCount = M.Map Integer (Integer, Integer)

findBingo :: Integer -> (Board, BingoCount, BingoCount) -> (Maybe Integer, BingoCount, BingoCount)
findBingo guess (board, rows, cols) = case M.lookup guess board of
  Nothing -> (Nothing, rows, cols)
  Just (x, y) -> case (M.lookup x rows, M.lookup y cols) of
    (Just (count, _), _) | count == 4 -> (Just $ calc guess, M.alter f x rows, cols)
    (_, Just (count, _)) | count == 4 -> (Just $ calc guess, rows, M.alter f y cols)
    (_, _) -> (Nothing, M.alter f x rows, M.alter f y cols)
    where
      calc :: Integer -> Integer
      calc n = (sum (M.keys board) - (M.foldl g 0 rows + n)) * n
        where
          g a (_, b) = a + b

      f :: Maybe (Integer, Integer) -> Maybe (Integer, Integer)
      f Nothing = Just (1, guess)
      f (Just (n, s)) = Just (n + 1, s + guess)

part1 :: String -> Integer
part1 input = go guesses (map (,M.empty,M.empty) boards)
  where
    go :: [Integer] -> [(Board, BingoCount, BingoCount)] -> Integer
    go [] _ = error "Didn't find an answer"
    go (x : next) bs = case foldl' f (Left []) bs of
      Right n -> n
      Left bs' -> go next bs'
      where
        f (Right n) _ = Right n
        f (Left updated) board = case findBingo x board of
          (Just ans, _, _) -> Right ans
          (Nothing, rows, cols) -> Left $ updated ++ [(fst3 board, rows, cols)]

    (guesses, boards) = case lines input of
      [] -> error "Unexpected empty lines"
      (x : xs) -> (strToInt <$> splitOn "," x, parseBoard . drop 1 <$> chunksOf 6 xs)

part2 :: String -> Integer
part2 input = go guesses (Nothing, map (,M.empty,M.empty) boards)
  where
    go :: [Integer] -> (Maybe Integer, [(Board, BingoCount, BingoCount)]) -> Integer
    go [] (Nothing, _) = error "Didn't find an answer"
    go [] (Just ans, _) = ans
    go (x : next) (mn, bs) = go next $ foldl' f (mn, []) bs
      where
        f (mn', updated) board = case findBingo x board of
          (Just ans, _, _) -> (Just ans, updated)
          (Nothing, rows, cols) -> (mn', updated ++ [(fst3 board, rows, cols)])

    (guesses, boards) = case lines input of
      [] -> error "Unexpected empty lines"
      (x : xs) -> (strToInt <$> splitOn "," x, parseBoard . drop 1 <$> chunksOf 6 xs)

main :: IO ()
main = do
  input <- readInput 4
  print (part1 input)
  print (part2 input)
