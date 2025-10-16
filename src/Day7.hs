module Day7 (day7) where

import Control.Monad (void)
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP (ReadP, char, eof, many1, munch1, readP_to_S, (<++))

data Equation = Equation
  { result :: Int,
    params :: [Int]
  }
  deriving (Show)

parseInput :: String -> [Equation]
parseInput = fst . head . readP_to_S parser
  where
    pEndOfLine :: ReadP ()
    pEndOfLine = void (char '\n') <++ eof

    pEquation :: ReadP Equation
    pEquation =
      Equation
        <$> ((read <$> munch1 isDigit) <* char ':')
        <*> many1 (char ' ' *> (read <$> munch1 isDigit))
        <* pEndOfLine

    parser :: ReadP [Equation]
    parser = many1 pEquation <* eof

validate :: [Int -> Int -> Int] -> Equation -> Bool
validate ops (Equation r ps) = validate' ps
  where
    validate' [] = r == 0
    validate' [x] = r == x
    validate' (x : _) | r < x = False
    validate' (x : y : zs) = any (\op -> validate' (op x y : zs)) ops

part1 :: [Equation] -> Int
part1 = sum . map result . filter (validate [(+), (*)])

part2 :: [Equation] -> Int
part2 = sum . map result . filter (validate [(+), (*), cat])
  where
    cat x y = read (show x ++ show y)

day7 :: IO ()
day7 = do
  input <- parseInput <$> readFile "puzzle-input/day7"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
