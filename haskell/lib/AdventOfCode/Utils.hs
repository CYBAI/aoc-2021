{-# LANGUAGE TypeApplications #-}

module AdventOfCode.Utils
  ( readInput,
    strToInt,
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

strToInt :: String -> Integer
strToInt = read @Integer
