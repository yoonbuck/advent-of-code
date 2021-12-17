module Day12 where

import Data.Char (isUpper)
import Data.Function ((&))
import Data.Graph (path)
import Data.List (elemIndex)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set

splitParagraphs :: String -> [String]
splitParagraphs = splitOn "\n\n"

type Node = String

type Graph = (Set.Set Node, Map.Map Node [Node])

-- a cave (node) is large if its name starts with a capital letter
isLarge :: Node -> Bool
isLarge = isUpper . head

canVisitB :: [Node] -> Node -> Bool
canVisitB path node
  | isLarge node = True
  | node == "start" = False
  | otherwise =
    if hasRepeatingElement (filter (not . isLarge) path)
      then node `notElem` path
      else isNotInOrOnce path node

-- return true if any element is in the list more than once
hasRepeatingElement :: (Ord a) => [a] -> Bool
hasRepeatingElement xs = (xs & Set.fromList & Set.toList & length) /= length xs

isNotInOrOnce :: [Node] -> Node -> Bool
isNotInOrOnce path node = case node `elemIndex` path of
  Nothing -> True
  Just n -> node `notElem` drop (succ n) path

-- type definitions

-- parsed input type
type Input = Graph

-- parse string input into input type
parse :: String -> Input
parse = foldl buildGraph emptyGraph . map parseLine . lines

parseLine :: String -> (String, String)
parseLine line = let [a, b] = splitOn "-" line in (a, b)

buildGraph :: Graph -> (String, String) -> Graph
buildGraph (nodes, edges) (from, to) =
  ( nodes & Set.insert from & Set.insert to,
    edges & Map.insertWith (++) from [to] & Map.insertWith (++) to [from]
  )

emptyGraph :: Graph
emptyGraph = (Set.empty, Map.empty)

getPathCountA :: [Node] -> Input -> Int
getPathCountA ("end" : _) _ = 1
getPathCountA path graph@(_, edges) =
  let potentialNextSteps = edges & Map.findWithDefault [] (head path)
      nextSteps = filter (\cave -> isLarge cave || cave `notElem` path) potentialNextSteps
   in sum $ map (\cave -> getPathCountA (cave : path) graph) nextSteps

getPathCountB :: [Node] -> Input -> Int
getPathCountB ("end" : _) _ = 1
getPathCountB path graph@(_, edges) =
  let potentialNextSteps = edges & Map.findWithDefault [] (head path)
      nextSteps = filter (canVisitB path) potentialNextSteps
   in sum $ map (\cave -> getPathCountB (cave : path) graph) nextSteps

-- solve the problem (part A)
solveA :: Input -> Int
solveA = getPathCountA ["start"]

--

-- solve the problem (part B)
solveB :: Input -> Int
solveB = getPathCountB ["start"]