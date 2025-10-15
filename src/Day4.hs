module Day4 (day4) where

import Control.Monad (guard)

part1 :: [[Char]] -> Int
part1 grid = sum $ do
  i <- [0 .. n - 1]
  j <- [0 .. m - 1]
  (dx, dy) <- [(dx, dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], (dx, dy) /= (0, 0)]
  guard $ and [inBounds (i + k * dx) (j + k * dy) | k <- [0 .. 3]]
  let str = [grid !! (i + k * dx) !! (j + k * dy) | k <- [0 .. 3]]
  guard $ str == "XMAS"
  return 1
  where
    n = length grid
    m = length (head grid)
    inBounds x y = 0 <= x && x < n && 0 <= y && y < m

part2 :: [[Char]] -> Int
part2 grid = sum $ do
  i <- [0 .. n - 1]
  j <- [0 .. m - 1]
  guard $ and [inBounds (i + dx) (j + dy) | (dx, dy) <- diagA ++ diagB]
  let strA = [grid !! (i + dx) !! (j + dy) | (dx, dy) <- diagA]
      strB = [grid !! (i + dx) !! (j + dy) | (dx, dy) <- diagB]
  guard $ (strA == "MAS" || strA == "SAM") && (strB == "MAS" || strB == "SAM")
  return 1
  where
    n = length grid
    m = length (head grid)
    diagA = [(-1, -1), (0, 0), (1, 1)]
    diagB = [(-1, 1), (0, 0), (1, -1)]
    inBounds x y = 0 <= x && x < n && 0 <= y && y < m

day4 :: IO ()
day4 = do
  input <- lines <$> readFile "puzzle-input/day4"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
