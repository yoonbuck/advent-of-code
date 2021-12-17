module Day13 where

import Data.List.Split (splitOn)
import qualified Data.Set as Set

splitParagraphs :: String -> [String]
splitParagraphs = splitOn "\n\n"

-- type definitions
type Point = (Int, Int)

data Axis = XAxis | YAxis deriving (Show, Eq)

type Fold = (Axis, Int)

-- parsed input type
type Input = (Set.Set Point, [Fold])

-- parse string input into input type
parse :: String -> Input
parse input =
  let [dots, folds] = splitParagraphs input
      dots' = map parseDot $ lines dots
      folds' = map parseFold $ lines folds
   in (Set.fromList dots', folds')

parseFold :: String -> Fold
parseFold = (\(dir : _ : rest) -> (parseDir dir, read rest)) . drop 11

parseDir :: Char -> Axis
parseDir 'x' = XAxis
parseDir 'y' = YAxis
parseDir _ = error "unexpected direction"

parseDot :: String -> Point
parseDot dot =
  let [x, y] = map read $ splitOn "," dot
   in (x, y)

fold :: Fold -> Point -> Point
fold (XAxis, k) (x, y) = (k - abs (x - k), y)
fold (YAxis, k) (x, y) = (x, k - abs (y - k))

--
foldAll :: Set.Set Point -> Fold -> Set.Set Point
foldAll dots f = Set.map (fold f) dots

-- solve the problem (part A)
solveA :: Input -> Int
solveA (dots, firstFold : _) = Set.size $ Set.map (fold firstFold) dots
solveA _ = error "expected at least one fold"

--

-- solve the problem (part B)
solveB :: Input -> String
solveB (dots, folds) =
  let afterAllFolds = foldl foldAll dots folds
   in "\n" ++ display afterAllFolds

display :: Set.Set Point -> String
display points =
  let -- get the max x and y of the points
      maxX = maximum $ map fst $ Set.toList points
      maxY = maximum $ map snd $ Set.toList points
   in unlines
        [ [if (x, y) `Set.member` points then '#' else '.' | x <- [0 .. maxX]]
          | y <- [0 .. maxY]
        ]
