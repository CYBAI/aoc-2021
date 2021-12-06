module AdventOfCode.Utils
  ( readInput,
  )
where

import System.Directory (getCurrentDirectory)

readInput :: Integer -> IO String
readInput day = do
  currentDir <- getCurrentDirectory
  readFile $ currentDir ++ "/inputs/Day" ++ pad day
  where
    pad :: Integer -> String
    pad d
      | d < 10 = "0" ++ show d
      | otherwise = show d
