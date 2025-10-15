module Day5 where

import Data.List (sortBy)
import Data.List.Split (splitOn)
import qualified Data.Set as S

mid :: [Int] -> Int
mid xs = xs !! (length xs `div` 2)

parseInput :: String -> (S.Set (Int, Int), [[Int]])
parseInput = (,) <$> S.fromList . map parseRule . filter ('|' `elem`) . lines <*> map parseUpdate . filter (',' `elem`) . lines
  where
    parseRule line =
      case splitOn "|" line of
        [a, b] -> (read a, read b)
        _ -> error $ "Invalid rule line: " ++ line
    parseUpdate = map read . splitOn ","

sortUpdates :: S.Set (Int, Int) -> [Int] -> ([Int], [Int])
sortUpdates rules = (,) <$> id <*> sortBy comparator
  where
    comparator a b
      | (a, b) `S.member` rules = LT
      | (b, a) `S.member` rules = GT
      | otherwise = EQ

part1 :: (S.Set (Int, Int), [[Int]]) -> Int
part1 (rules, updates) = sum . map (mid . snd) . filter (uncurry (==)) . map (sortUpdates rules) $ updates

part2 :: (S.Set (Int, Int), [[Int]]) -> Int
part2 (rules, updates) = sum . map (mid . snd) . filter (uncurry (/=)) . map (sortUpdates rules) $ updates

day5 :: IO ()
day5 = do
  input <- parseInput <$> readFile "puzzle-input/day5"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
