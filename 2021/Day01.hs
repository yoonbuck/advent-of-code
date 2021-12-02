module Day01 where

getIsIncreasing :: [Int] -> [Bool]
getIsIncreasing xs = zipWith (<) xs (tail xs)

parse :: String -> [Int]
parse = map read . lines

countIncreasing :: [Int] -> Int
countIncreasing = length . filter id . getIsIncreasing

solveA :: [Int] -> Int
solveA = countIncreasing

makeSlidingWindows :: [Int] -> [(Int, Int, Int)]
makeSlidingWindows xs = zip3 xs (tail xs) (tail $ tail xs)

sumWindow :: (Int, Int, Int) -> Int
sumWindow (x, y, z) = x + y + z

solveB :: [Int] -> Int
solveB = countIncreasing . map sumWindow . makeSlidingWindows

main = interact ((++ "\n") . show . solveB . parse)