module Day08 where

import Data.List.Extra (dropEnd, nub)
import Data.List.Split (splitOn)
import Data.Map qualified as M

-- type definitions
data Direction = L | R deriving (Show)

parseDirection :: Char -> Direction
parseDirection 'L' = L
parseDirection 'R' = R

type Network = M.Map String (String, String)

parseNode :: String -> (String, (String, String))
parseNode nodeStr = (nN, (nL, nR))
  where
    [nN, nV] = splitOn " = " nodeStr
    [nL', nR'] = splitOn ", " nV
    nL = tail nL'
    nR = dropEnd 1 nR'

-- parsed input type
type Input = ([Direction], Network)

-- parse string input into input type
parse :: String -> Input
parse input =
  let [directions, network] = splitOn "\n\n" input
      directions' = map parseDirection directions
      network' = M.fromList $ map parseNode $ lines network
   in (directions', network')

--

step :: Network -> Direction -> String -> String
step network direction node =
  let (l, r) = network M.! node
   in case direction of
        L -> l
        R -> r

-- solve the problem (part A)
solveA :: Input -> Int
solveA (directions, network) = go 0 (cycle directions) "AAA"
  where
    go :: Int -> [Direction] -> String -> Int
    go acc _ "ZZZ" = acc
    go acc (d : ds) node = go (acc + 1) ds (step network d node)

--

endNode :: String -> Bool
endNode = (== 'Z') . last

startNode :: String -> Bool
startNode = (== 'A') . last

startingNodes :: Network -> [String]
startingNodes network = filter startNode $ M.keys network

-- for manual inspection of cycles
getEndpoints :: Network -> [Direction] -> String -> [(String, Int)]
getEndpoints net ds = go 0 (cycle ds)
  where
    go :: Int -> [Direction] -> String -> [(String, Int)]
    go acc (dir : dirs) node
      | endNode node = (node, acc) : rest
      | otherwise = rest
      where
        rest = go (acc + 1) dirs (step net dir node)

-- |
-- getCycleLength assumes that:
-- 1. each "ghost" hits only one end node
-- 2. the amount of time it takes to reach an end node from the start is the same amount of
--    time it takes to reach the end node again from itself
--
-- These appear to be true for the input - determined interactively with getEndpoints, e.g.,
-- >>> take 5 $ getEndpoints network directions "AAA"
-- [("ZZZ", 13939), ("ZZZ", 27878), ("ZZZ", 41817), ("ZZZ", 55756), ("ZZZ", 69695)]
getCycleLength :: Network -> [Direction] -> String -> Int
getCycleLength net ds = snd . head . getEndpoints net ds

-- solve the problem (part B)
solveB :: Input -> Int
solveB (directions, network) = flcm $ map (getCycleLength network directions) $ startingNodes network
  where
    flcm :: [Int] -> Int
    flcm = foldl1 lcm