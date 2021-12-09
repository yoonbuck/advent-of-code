module Day09 where

import Data.Function ((&))
import Data.List (sort)
import qualified Data.Set as Set

-- type definitions
-- position: (x, y)
type Position = (Int, Int)

-- parsed input type
type Input = [[Int]]

-- parse string input into input type
parse :: String -> Input
parse = map (map (read . (: []))) . lines

depthAt :: Input -> Position -> Int
depthAt input (x, y) = input !! y !! x

getNeighbors :: Position -> [Position]
getNeighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

getSize :: Input -> (Int, Int)
getSize input = (width, height)
  where
    width = length (head input)
    height = length input

inBounds :: Input -> Position -> Bool
inBounds input (x, y) = x >= 0 && x < width && y >= 0 && y < height
  where
    (width, height) = getSize input

--

-- solve the problem (part A)
solveA :: Input -> Int
solveA input = sum $ map (succ . depthAt') $ filter isLowPoint options
  where
    (width, height) = getSize input
    options = [(x, y) | x <- [0 .. width - 1], y <- [0 .. height - 1]]
    depthAt' = depthAt input
    inBounds' = inBounds input
    isLowPoint (x, y) =
      let neighbors = filter inBounds' $ getNeighbors (x, y)
          depthAtPoint = depthAt' (x, y)
       in all ((> depthAtPoint) . depthAt') neighbors

--

type Basin = [Position]

-- solve the problem (part B)
{-

1. get all of the lowest points
2. expand each of those points to that point's "basin"
3. take the largest three basins
    The size of the basin is the number of locations within the basin, including the low point.
4. multiply the sizes of the three basins

-}
solveB :: Input -> Int
solveB input = product $ take 3 $ reverse $ sort $ map length basins
  where
    (width, height) = getSize input
    options = [(x, y) | x <- [0 .. width - 1], y <- [0 .. height - 1]]
    depthAt' = depthAt input
    inBounds' = inBounds input
    isLowPoint (x, y) =
      let neighbors = filter inBounds' $ getNeighbors (x, y)
          depthAtPoint = depthAt' (x, y)
       in all ((> depthAtPoint) . depthAt') neighbors
    lowPoints = filter isLowPoint options
    basins = map (expandBasin input) lowPoints

-- State: (candidates, pointsInBasin, pointsExploredButNotInBasin)
type State = (Set.Set Position, Set.Set Position, Set.Set Position)

-- starting at a single point, "flood-fill" the basin, stopping at points where the depth = 9
expandBasin :: Input -> Position -> Basin
expandBasin input position = go (Set.fromList [position], Set.empty, Set.empty)
  where
    depthAt' = depthAt input
    inBounds' = inBounds input
    go :: State -> Basin
    go (candidates, pointsInBasin, pointsExploredButNotInBasin)
      | Set.null candidates = Set.toList pointsInBasin
      | otherwise =
        if depthAt' candidate == 9
          then go (newCandidates, pointsInBasin, Set.insert candidate pointsExploredButNotInBasin)
          else go (candidatesWithNeighbors, Set.insert candidate pointsInBasin, pointsExploredButNotInBasin)
      where
        (candidate, newCandidates) = Set.deleteFindMin candidates
        newPotentialCandidates = Set.fromList $ filter inBounds' $ getNeighbors candidate
        candidatesWithNeighbors =
          newPotentialCandidates
            & (`Set.difference` pointsExploredButNotInBasin)
            & (`Set.difference` pointsInBasin)
            & Set.union newCandidates
