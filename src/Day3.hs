{-# LANGUAGE OverloadedStrings #-}

module Day3 (day3) where

import Text.Regex.TDFA (AllTextMatches (getAllTextMatches), (=~))

data Command = Do | Mul Int Int | Dont

parseInput :: String -> [Command]
parseInput input = map toCommand (flip (=~) mulPattern <$> allMatches :: [(String, String, String, [String])])
  where
    mulPattern = "(do\\(\\))|(don't\\(\\))|(mul\\(([0-9]+),([0-9]+)\\))" :: String
    allMatches = getAllTextMatches (input =~ mulPattern) :: [String]
    toCommand (_, _, _, [a, b, _, c, d])
      | a == "do()" = Do
      | b == "don't()" = Dont
      | otherwise = Mul (read c) (read d)
    toCommand _ = error "Invalid command"

part1 :: [Command] -> Int
part1 input = sum . (uncurry (*) <$>) $ [(a, b) | Mul a b <- input]

part2 :: [Command] -> Bool -> Int
part2 (Do : xs) _ = part2 xs True
part2 (Dont : xs) _ = part2 xs False
part2 (Mul a b : xs) True = a * b + part2 xs True
part2 (Mul _ _ : xs) False = part2 xs False
part2 [] _ = 0

day3 :: IO ()
day3 = do
  input <- parseInput <$> readFile "puzzle-input/day3"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input True)
