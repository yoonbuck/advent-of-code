module Day12 where

import Data.Bifunctor (bimap)
import Data.List (intercalate, intersperse)
import Data.List.Split (splitOn)

-- type definitions
data SpringStatus
  = Operational
  | Damaged
  | Unknown
  deriving (Show)

parseSpring :: Char -> SpringStatus
parseSpring '.' = Operational
parseSpring '#' = Damaged
parseSpring '?' = Unknown

type DamageSizes = [Int]

-- parsed input type
type Input = [([SpringStatus], DamageSizes)]

-- parse string input into input type
parse :: String -> Input
parse = map parseLine . lines

parseLine :: String -> ([SpringStatus], DamageSizes)
parseLine input =
  let [springs, status] = words input
      springStatus = map parseSpring springs
      damageSizes = map read $ splitOn "," status
   in (springStatus, damageSizes)

getSolutionCount :: ([SpringStatus], DamageSizes) -> Int
getSolutionCount = uncurry $ getSolutionCount' False

getSolutionCount' ::
  Bool -> -- If we are inside a span of damaged springs
  [SpringStatus] ->
  DamageSizes ->
  Int
-- Reached the end and no more damaged springs to find - done
getSolutionCount' _ [] [] = 1
getSolutionCount' _ [] [0] = 1
-- Reached the end but expecting more damaged springs - not a solution
getSolutionCount' _ [] _ = 0
-- Found an unknown spring - try both options
getSolutionCount' b (Unknown : rest) damageSizes =
  getSolutionCount' b (Operational : rest) damageSizes + getSolutionCount' b (Damaged : rest) damageSizes
-- Ending a span of damaged springs: require an operational spring
getSolutionCount' True (Operational : rest) (0 : damageSizes) =
  getSolutionCount' False rest damageSizes
-- If otherwise in a span of damaged springs, require a damaged spring
getSolutionCount' True (Damaged : rest) (n : damageSizes) =
  getSolutionCount' True rest (n - 1 : damageSizes)
-- Otherwise, in a span of damaged springs, reject this solution.
getSolutionCount' True _ _ = 0
-- Starting a span of damaged springs with a damaged spring
getSolutionCount' False (Damaged : rest) (n : damageSizes) =
  getSolutionCount' True rest (n - 1 : damageSizes)
-- Avoiding a span of damaged springs with a operational spring
getSolutionCount' False (Operational : rest) damageSizes =
  getSolutionCount' False rest damageSizes
-- Otherwise, if a damaged spring appears, but no spans remain, reject this solution.
getSolutionCount' False (Damaged : _) [] = 0

-- solve the problem (part A)
solveA :: Input -> Int
solveA = sum . map getSolutionCount

--

-- solve the problem (part B)
solveB :: Input -> Int
solveB = sum . map (getSolutionCount . bimap unfoldConditions unfoldGroups) . take 1

unfoldConditions :: [SpringStatus] -> [SpringStatus]
unfoldConditions = intercalate [Unknown] . replicate 5

unfoldGroups :: [a] -> [a]
unfoldGroups = concat . replicate 5
