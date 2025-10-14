module Day2 (day2) where

parseInput :: String -> [[Int]]
parseInput = map (map read . words) . lines

deleteAtIndex :: Int -> [a] -> [a]
deleteAtIndex n xs = take n xs ++ drop (n + 1) xs

inc :: Int -> Int -> Bool
inc x y = x < y && y <= x + 3

dec :: Int -> Int -> Bool
dec x y = inc y x

safe :: [Int] -> Bool
safe x = safeInDirection inc x || safeInDirection dec x
  where
    safeInDirection dir (x1 : x2 : xs) = dir x1 x2 && safeInDirection dir (x2 : xs)
    safeInDirection _ _ = True

part1 :: [[Int]] -> Int
part1 = length . filter safe

part2 :: [[Int]] -> Int
part2 = length . filter almostSafe
  where
    almostSafe :: [Int] -> Bool
    almostSafe xs = safe xs || any (safe . flip deleteAtIndex xs) [0 .. length xs - 1]

day2 :: IO ()
day2 = do
  input <- parseInput <$> readFile "puzzle-input/day2"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
