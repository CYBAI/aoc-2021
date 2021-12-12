import AdventOfCode.Utils
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

main :: IO ()
main = do
  input <- readInput 8
  print (part1 input)
