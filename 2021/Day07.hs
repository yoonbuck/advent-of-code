module Day07 where

import Data.List.Split (splitOn)

-- parsed input type
type Input = [Int]

-- parse string input into input type
parse :: String -> Input
parse = map read . splitOn ","

getDistance :: Int -> [Int] -> [Int]
getDistance target = map (abs . (target -))

getFuelCostA :: Int -> [Int] -> Int
getFuelCostA target = sum . getDistance target

fuelCostB :: Int -> Int
fuelCostB n = n * (n + 1) `div` 2

getFuelCostB :: Int -> [Int] -> Int
getFuelCostB target = sum . map fuelCostB . getDistance target

-- solve the problem (part A)
solveA :: Input -> Int
solveA input = minimum $ map (`getFuelCostA` input) [0 .. 1400]

-- solve the problem (part B)
solveB :: Input -> Int
solveB input = minimum $ map (`getFuelCostB` input) [0 .. 1400]