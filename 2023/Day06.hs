module Day06 (Input, parse, solveA, solveB) where

import Data.Tuple.Extra (both)

-- type definitions
type Segment = (Int, Int)

-- parsed input type
type Input = [Segment]

-- parse string input into input type
parse :: String -> Input
parse input =
  let [t, d] = lines input
      times = map read $ tail $ words t
      distances = map read $ tail $ words d
   in zip times distances

-- see derivation in Day06.pdf
solveSegment :: (Int, Int) -> Int
solveSegment (timeI, distance) =
  ceiling ((time + sqdisc) / 2) - floor ((time - sqdisc) / 2) - 1
  where
    time = fromIntegral timeI
    sqdisc = sqrt $ fromIntegral $ timeI ^ 2 - 4 * distance

-- solve the problem (part A)
solveA :: Input -> Int
solveA = product . map solveSegment

reparseB :: Input -> Input
reparseB = (: []) . both (read . concatMap show) . unzip

-- solve the problem (part B)
solveB :: Input -> Int
solveB = product . map solveSegment . reparseB
