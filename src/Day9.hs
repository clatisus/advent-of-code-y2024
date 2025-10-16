{-# LANGUAGE NamedFieldPuns #-}

module Day9 (day9) where

import Data.Char (digitToInt)
import Data.List.Split (chunksOf)
import qualified Data.Vector as V

type Space = (Int, Int) -- [start, end] both inclusive

type File = (Space, Int) -- (space, ID of the file)

data Disk = Disk
  { freeSpace :: V.Vector Space,
    usedSpace :: [File]
  }
  deriving (Show)

addFree :: [Space] -> Disk -> Disk
addFree free Disk {freeSpace, usedSpace} = Disk {freeSpace = freeSpace V.++ V.fromList free, usedSpace = usedSpace}

addUsed :: [File] -> Disk -> Disk
addUsed used Disk {freeSpace, usedSpace} = Disk {freeSpace = freeSpace, usedSpace = used ++ usedSpace}

checksum :: [File] -> Int
checksum = sum . concatMap (\((s, e), idx) -> [i * idx | i <- [s .. e]])

fragmentate :: Space -> [Space]
fragmentate (s, e) = map ((,) <$> head <*> last) $ chunksOf 1 [s .. e]

toDisk :: (Space -> [Space]) -> String -> Disk
toDisk frag diskMap = goUsed diskMapWithId 0 (Disk V.empty [])
  where
    diskMapWithId = zip (map digitToInt diskMap) (concatMap (replicate 2) [0 :: Int ..])

    goFree :: [(Int, Int)] -> Int -> Disk -> Disk
    goFree ((cnt, _) : xs) n d
      | cnt == 0 = goUsed xs n d
      | otherwise = goUsed xs (n + cnt) $ addFree (frag (n, n + cnt - 1)) d
    goFree [] _ disk = disk

    goUsed :: [(Int, Int)] -> Int -> Disk -> Disk
    goUsed ((cnt, idx) : xs) n d
      | cnt == 0 = goFree xs n d
      | otherwise =
          goFree xs (n + cnt) $
            addUsed (zip (reverse (frag (n, n + cnt - 1))) (replicate cnt idx)) d
    goUsed [] _ disk = disk

fillDisk :: [File] -> Disk -> ([File], Disk)
fillDisk relocated d@Disk {freeSpace = _, usedSpace = []} = (relocated, d)
fillDisk relocated Disk {freeSpace = fs, usedSpace = (u@((us, ue), idx) : ur)} =
  case V.findIndex predicate fs of
    Nothing -> fillDisk (u : relocated) Disk {freeSpace = fs, usedSpace = ur}
    Just i ->
      let (s, e) = fs V.! i
          relocated' = ((s, s + lenUsed - 1), idx) : relocated
          fs' = fs V.// [(i, (s + lenUsed, e))]
       in fillDisk relocated' Disk {freeSpace = fs', usedSpace = ur}
  where
    lenUsed = ue - us + 1
    predicate (s, e) = s < us && e - s + 1 >= lenUsed

part1 :: String -> Int
part1 = checksum . ((++) <$> fst <*> usedSpace . snd) . fillDisk [] . toDisk fragmentate

part2 :: String -> Int
part2 = checksum . ((++) <$> fst <*> usedSpace . snd) . fillDisk [] . toDisk (: [])

day9 :: IO ()
day9 = do
  input <- filter ('\n' /=) <$> readFile "puzzle-input/day9"
  putStrLn $ "part 1: " ++ show (part1 input)
  putStrLn $ "part 2: " ++ show (part2 input)
