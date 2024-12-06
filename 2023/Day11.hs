module Day11 where

import Data.List (transpose)
import Data.List.Split (splitOn)
import GHC.Arr qualified as A

-- type definitions

-- parsed input type
type Input = [[Bool]]

-- parse string input into input type
parse :: String -> Input
parse = map (map (== '#')) . lines

--

expand :: Input -> Input
expand [] = []
expand (row : rows)
  | not $ or row = row : row : expand rows
  | otherwise = row : expand rows

-- solve the problem (part A)
solveA :: Input -> Int
solveA input =
  let expanded = expand . transpose . expand $ input
      arr = A.listArray ((0, 0), (length expanded - 1, length (head expanded) - 1)) $ concat expanded
   in sum $ distances arr

distances :: A.Array (Int, Int) Bool -> [Int]
distances arr =
  let arrList = filter snd (A.assocs arr)
   in map (uncurry manhattan) $ pairs $ map fst arrList

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = [(x, y) | y <- xs] ++ pairs xs

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

--

-- solve the problem (part B)
solveB :: Input -> Int
solveB = undefined
