module Day09 where

-- parsed input type
type Input = [[Int]]

-- parse string input into input type
parse :: String -> Input
parse = map (map read . words) . lines

--
extrapolate :: [Int] -> Int
extrapolate vs
  | all (== 0) vs = 0
  | otherwise = last vs + extrapolate (zipWith (-) (tail vs) vs)

-- solve the problem (part A)
solveA :: Input -> Int
solveA = sum . map extrapolate

-- solve the problem (part B)
solveB :: Input -> Int
solveB = solveA . map reverse
