module Day01 (parse, Input, solveA, solveB) where

import           Data.Char       (isNumber)
import           Data.List.Extra (replace)

type Input = [String]

parse :: String -> [String]
parse = lines

solveA :: Input -> Int
solveA = sum . map solveLine

solveLine :: [Char] -> Int
solveLine line =
  let digitsOnly = filter isNumber line
      firstDigit = case digitsOnly of f:_ -> f
                                      _   -> '0'
      lastDigit = case digitsOnly of [] -> '0'
                                     _  -> last digitsOnly
  in read [firstDigit, lastDigit]

transformLine :: String -> String
transformLine =
  replace "one" "1" .
  replace "two" "2o" .
  replace "three" "3" .
  replace "four" "4" .
  replace "five" "5" .
  replace "six" "6" .
  replace "seven" "7" .
  replace "eight" "e8t" .
  replace "nine" "n9e"

solveB :: Input -> Int
solveB = sum . map (solveLine . transformLine)
