module Day1 (day1) where

import Data.List (sort)
import Data.List.Split (splitOn)

parseInput :: String -> ([Int], [Int])
parseInput input = (map fst pairs, map snd pairs)
  where
    pairs :: [(Int, Int)]
    pairs = (((,) <$> (read . head)) <*> read . last) . splitOn " " <$> lines input

part1 :: ([Int], [Int]) -> Int
part1 (xs, ys) = sum $ zipWith (\x y -> abs (x - y)) (sort xs) (sort ys)

part2 :: ([Int], [Int]) -> Int
part2 (xs, ys) = sum [x | x <- xs, y <- ys, x == y]

day1 :: IO ()
day1 = do
  input <- parseInput <$> readFile "puzzle-input/day1"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
