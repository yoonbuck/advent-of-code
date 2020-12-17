-- horrible redundant version :)
-- https://gist.github.com/yoonbuck/b567f882793baaac7056d86ceec63a13
{-# LANGUAGE FlexibleInstances #-}

import qualified Data.Set as Set

class (Ord a) => Cube a where
  neighbors :: a -> [a]

type Cube3 = (Int, Int, Int)

instance Cube Cube3 where
  neighbors (x, y, z) = [(x + dx, y + dy, z + dz)
                        | dx <- [-1 .. 1]
                        , dy <- [-1 .. 1]
                        , dz <- [-1 .. 1]]

type Cube4 = (Int, Int, Int, Int)

instance Cube Cube4 where
  neighbors (x, y, z, w) =
    [(x + dx, y + dy, z + dz, w + dw)
    | dx <- [-1 .. 1]
    , dy <- [-1 .. 1]
    , dz <- [-1 .. 1]
    , dw <- [-1 .. 1]]

parse3 :: String -> Set.Set Cube3
parse3 input =
  let inputLines = lines input
      cols = length $ head inputLines
      rows = length inputLines
  in Set.fromList
       [(x, y, 0)
       | x <- [0 .. pred cols]
       , y <- [0 .. pred rows]
       , inputLines !! y !! x == '#']

parse4 :: String -> Set.Set Cube4
parse4 input =
  let inputLines = lines input
      cols = length $ head inputLines
      rows = length inputLines
  in Set.fromList
       [(x, y, 0, 0)
       | x <- [0 .. pred cols]
       , y <- [0 .. pred rows]
       , inputLines !! y !! x == '#']

cubesToCheck :: (Cube c) => Set.Set c -> Set.Set c
cubesToCheck = Set.fromList . concatMap neighbors . Set.toList

activeNeighbors :: (Cube c) => Set.Set c -> c -> Int
activeNeighbors st = length . filter (`Set.member` st) . neighbors

shouldBeActive :: (Cube c) => Set.Set c -> c -> Bool
shouldBeActive st c =
  let n = activeNeighbors st c
  in if Set.member c st
     then n == 3 || n == 4 -- neighbor count includes current cube
     else n == 3

update :: (Cube c) => Set.Set c -> Set.Set c
update oldState = let eligibleSet = cubesToCheck oldState
                  in Set.filter (shouldBeActive oldState) eligibleSet

solve :: (Cube c) => Set.Set c -> Int
solve = Set.size . (!! 6) . iterate update

main :: IO ()
main = do
  input <- getContents
  putStrLn $ "Part A: " ++ (show . solve . parse3) input
  putStrLn $ "Part B: " ++ (show . solve . parse4) input
