module Day05 where

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tuple.Extra (both)

-- type definitions

type Point = (Int, Int)

type Line = (Point, Point)

lineIsVertical :: Line -> Bool
lineIsVertical ((x1, _), (x2, _)) = x1 == x2

lineIsHorizontal :: Line -> Bool
lineIsHorizontal ((_, y1), (_, y2)) = y1 == y2

-- parsed input type
type Input = [Line]

-- parse string input into input type
parse :: String -> Input
parse = map parseLine . lines

firstTwo :: [b] -> (b, b)
firstTwo (a : b : _) = (a, b)
firstTwo _ = error "firstTwo: not enough elements"

parseLine :: String -> Line
parseLine = firstTwo . map parsePoint . splitOn " -> "

parsePoint :: String -> Point
parsePoint = firstTwo . map read . splitOn ","

isHorizontalOrVertical :: Line -> Bool
isHorizontalOrVertical line = lineIsHorizontal line || lineIsVertical line

--

-- solve the problem (part A)
solveA :: Input -> Int
solveA input =
  let lines = filter isHorizontalOrVertical input
      coverage = foldl addLineCoverage Map.empty lines
   in length $ filter (\(_, v) -> v > 1) $ Map.toList coverage

type Coverage = Map.Map Point Int

getPointsCovered :: Line -> [Point]
getPointsCovered line@((x1, y1), (x2, y2))
  | lineIsVertical line = [(x1, y) | y <- if y1 < y2 then [y1 .. y2] else [y2 .. y1]]
  | lineIsHorizontal line = [(x, y1) | x <- if x1 < x2 then [x1 .. x2] else [x2 .. x1]]
  | otherwise = getDiagonalCoverage line

-- assumes: the line is exactly 45 degrees!
getDiagonalCoverage :: Line -> [Point]
getDiagonalCoverage ((x1, y1), (x2, y2)) =
  let dirx = if x1 < x2 then 1 else -1
      diry = if y1 < y2 then 1 else -1
      diff = abs $ x2 - x1
      x = x1
      y = y1
   in map (\d -> (x1 + (d * dirx), y1 + (d * diry))) [0 .. diff]

addPointCoverage :: Coverage -> Point -> Coverage
addPointCoverage coverage point =
  Map.insertWith (+) point 1 coverage

addLineCoverage :: Coverage -> Line -> Coverage
addLineCoverage pastCoverage = foldl addPointCoverage pastCoverage . getPointsCovered

-- solve the problem (part B)
solveB :: Input -> Int
solveB =
  length
    . filter (\(_, v) -> v > 1)
    . Map.toList
    . foldl addLineCoverage Map.empty