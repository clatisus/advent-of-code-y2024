module Day6 where

import Control.Monad (guard)
import qualified Data.Set as S
import qualified Data.Vector as V

data Dir = U | D | L | R deriving (Show, Eq, Ord)

move :: (Int, Int) -> Dir -> (Int, Int)
move (x, y) U = (x - 1, y)
move (x, y) D = (x + 1, y)
move (x, y) L = (x, y - 1)
move (x, y) R = (x, y + 1)

getDir :: Char -> Dir
getDir '^' = U
getDir 'v' = D
getDir '<' = L
getDir '>' = R
getDir _ = error "Invalid direction character"

nextDir :: Dir -> Dir
nextDir U = R
nextDir R = D
nextDir D = L
nextDir L = U

part1 :: V.Vector (V.Vector Char) -> Int -> Int -> (Int, Int) -> Int
part1 g n m (startX, startY) = S.size $ go (startX, startY) dir
  where
    dir = getDir (g V.! startX V.! startY)
    go (x, y) d
      | nx < 0 || nx >= n || ny < 0 || ny >= m = S.singleton (x, y)
      | g V.! nx V.! ny == '#' = go (x, y) (nextDir d)
      | otherwise = S.insert (x, y) (go (nx, ny) d)
      where
        (nx, ny) = move (x, y) d

part2 :: V.Vector (V.Vector Char) -> Int -> Int -> (Int, Int) -> Int
part2 g n m (startX, startY) = sum $ do
  i <- [0 .. n - 1]
  j <- [0 .. m - 1]
  guard $ g V.! i V.! j == '.'
  let newGrid = g V.// [(i, (g V.! i) V.// [(j, '#')])]
  guard $ checkGrid newGrid (startX, startY) dir S.empty
  return 1
  where
    dir = getDir (g V.! startX V.! startY)
    checkGrid grid (x, y) d visited
      | S.member (x, y, d) visited = True
      | nx < 0 || nx >= n || ny < 0 || ny >= m = False
      | grid V.! nx V.! ny == '#' = checkGrid grid (x, y) (nextDir d) (S.insert (x, y, d) visited)
      | otherwise = checkGrid grid (nx, ny) d (S.insert (x, y, d) visited)
      where
        (nx, ny) = move (x, y) d

day6 :: IO ()
day6 = do
  input <- V.fromList . map V.fromList . lines <$> readFile "puzzle-input/day6"
  let n = V.length input
      m = V.length (input V.! 0)
      start = head [(i, j) | i <- [0 .. n - 1], j <- [0 .. m - 1], input V.! i V.! j `elem` "^v<>"]
  putStrLn $ "Part 1: " ++ show (part1 input n m start)
  putStrLn $ "Part 2: " ++ show (part2 input n m start)
