{-# LANGUAGE NamedFieldPuns #-}

module Day8 (day8) where

import Data.Char (isAlphaNum)
import Data.Function (on)
import Data.List (groupBy, sortBy)
import qualified Data.Set as S

data Grid = Grid
  { n :: Int,
    m :: Int,
    antennas :: [(Char, [(Int, Int)])]
  }
  deriving (Show)

parseInput :: String -> Grid
parseInput input = Grid n m antennas
  where
    linesOfInput = lines input
    n = length linesOfInput
    m = length (head linesOfInput)
    antennas =
      filter (isAlphaNum . fst)
        . map ((,) <$> fst . head <*> map snd)
        . groupBy ((==) `on` fst)
        . sortBy (compare `on` fst)
        $ zip (concat linesOfInput) [(i, j) | i <- [0 .. n - 1], j <- [0 .. m - 1]]

countOccurs :: [Int] -> Grid -> Int
countOccurs steps Grid {n, m, antennas} = S.size allOccurs
  where
    inBound (x, y) = x >= 0 && x < n && y >= 0 && y < m
    occur (x, y) (x', y') =
      S.fromList
        ( ((++) `on` takeWhile inBound)
            [((i + 1) * x - i * x', (i + 1) * y - i * y') | i <- steps]
            [((i + 1) * x' - i * x, (i + 1) * y' - i * y) | i <- steps]
        )
    occurs ps = S.unions [occur p1 p2 | p1 <- ps, p2 <- ps, p1 /= p2]
    allOccurs = S.unions $ map (occurs . snd) antennas

part1 :: Grid -> Int
part1 = countOccurs [1]

part2 :: Grid -> Int
part2 = countOccurs [0 ..]

day8 :: IO ()
day8 = do
  input <- parseInput <$> readFile "puzzle-input/day8"
  putStrLn $ "part 1: " ++ show (part1 input)
  putStrLn $ "part 2: " ++ show (part2 input)
