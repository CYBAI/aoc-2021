import AdventOfCode.Utils
import Data.List (foldl', sort)
import qualified Data.Map.Strict as M
import Data.Set (Set, (\\))
import qualified Data.Set as S
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

unsafeFromRight :: Either a b -> b
unsafeFromRight (Right x) = x
unsafeFromRight (Left _) = error "Unexpected Left value"

strP :: Parser [String]
strP = do
  xs <- P.many P.letter `P.sepBy` P.string " "
  return $ filter (/= "") xs

type Line = ([String], [String])

lineP :: Parser Line
lineP = do
  pats <- strP
  _ <- P.string "|"
  outputs <- strP
  return (pats, outputs)

parse :: String -> [Line]
parse = (unsafeFromRight . P.parse lineP "Line Parser" <$>) . lines

part1 :: String -> Int
part1 = sum . (length . filter checkLen . snd <$>) . parse
  where
    checkLen :: String -> Bool
    checkLen str = case length str of
      2 -> True -- 1
      3 -> True -- 4
      4 -> True -- 7
      7 -> True -- 8
      _ -> False

transform :: [String] -> M.Map Int (Set (Set Char))
transform = go M.empty
  where
    go hm [] = hm
    go hm (x : xs) = go (M.alter f (length x) hm) xs
      where
        f Nothing = Just $ S.singleton (S.fromList x)
        f (Just s) = Just $ S.insert (S.fromList x) s

decrypt :: M.Map Int (Set (Set Char)) -> [Char]
decrypt hm = [zero, one, two, three, four, five, six]
  where
    first = S.elemAt 0

    zero = first $ digitSeven \\ digitOne
    four = first $ digitEight \\ digitNine
    six = first $ first $ S.filter ((1 ==) . S.size) $ S.map (\\ noBottomDigitNine) (hm M.! 6)
    (two, three) =
      let (stwo, sthree) = S.partition (\c -> S.member c digitOne) $ S.map (first . (digitEight \\)) $ S.delete digitNine (hm M.! 6)
       in (first stwo, first sthree)
    five = first $ digitOne \\ S.singleton two
    one = first $ digitFour \\ (S.fromList [two, three, five])

    noBottomDigitNine = S.insert zero (first $ hm M.! 4)

    digitOne = first (hm M.! 2)
    digitFour = first (hm M.! 4)
    digitSeven = first (hm M.! 3)
    digitEight = first (hm M.! 7)
    digitNine = S.insert six noBottomDigitNine

toDigit :: [Integer] -> Integer
toDigit [0, 1, 2, 4, 5, 6] = 0
toDigit [2, 5] = 1
toDigit [0, 2, 3, 4, 6] = 2
toDigit [0, 2, 3, 5, 6] = 3
toDigit [1, 2, 3, 5] = 4
toDigit [0, 1, 3, 5, 6] = 5
toDigit [0, 1, 3, 4, 5, 6] = 6
toDigit [0, 2, 5] = 7
toDigit [0, 1, 2, 3, 4, 5, 6] = 8
toDigit [0, 1, 2, 3, 5, 6] = 9
toDigit _ = error "Not supported digit"

findPos :: [Char] -> [String] -> [Integer]
findPos _ [] = []
findPos cs (str : xs) = go str : (findPos cs xs)
  where
    go s = toDigit $ sort (charIndex <$> s)

    indexes :: M.Map Char Integer
    indexes = f M.empty 0 cs
      where
        f :: M.Map Char Integer -> Integer -> [Char] -> M.Map Char Integer
        f hm _ [] = hm
        f hm i (cc : cs') = f (M.insert cc i hm) (i + 1) cs'

    charIndex :: Char -> Integer
    charIndex c = case M.lookup c indexes of
      Just x -> x
      Nothing -> error "Unexpected character"

solve :: Line -> Integer
solve (pats, outputs) = strToInt $ foldl' (++) "" (show <$> findPos (decrypt $ transform pats) outputs)

part2 :: String -> Integer
part2 input = sum $ solve <$> parse input

main :: IO ()
main = do
  input <- readInput 8
  print (part1 input)
  print (part2 input)
