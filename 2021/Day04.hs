module Day04 where

import Data.List (transpose)
import Data.List.Split (chunksOf, split, splitOn)

splitParagraphs :: String -> [String]
splitParagraphs = splitOn "\n\n"

-- type definitions

data Cell = Cell
  { value :: Int,
    marked :: Bool
  }
  deriving (Eq, Show)

printCell :: Cell -> String
printCell (Cell v m) = show v ++ if m then "*" else ""

type Board = [Cell]

type Draws = [Int]

rows :: [[Int]]
rows = [map (+ n) [0 .. 4] | n <- [0, 5 .. 20]]

bingoIndices :: [[Int]]
bingoIndices = rows ++ transpose rows

boardHasBingo :: Board -> Bool
boardHasBingo board = any (all (marked . (board !!))) bingoIndices

mark :: Int -> Board -> Board
mark v = map markCell
  where
    markCell cell = if value cell == v then cell {marked = True} else cell

-- parsed input type
type Input = (Draws, [Board])

-- parse string input into input type
parse :: String -> Input
parse rawInput = (draws, board)
  where
    drawsSection : boards = splitParagraphs rawInput
    draws = map read $ splitOn "," drawsSection
    board = map (map ((`Cell` False) . read) . words) boards

sumOfUnmarked :: Board -> Int
sumOfUnmarked = sum . map value . filter (not . marked)

-- solve the problem (part A)
solveA :: Input -> Int
solveA (draws, boards) =
  let draw : nextDraws = draws
      updatedBoards = map (mark draw) boards
   in if any boardHasBingo updatedBoards
        then sumOfUnmarked (head $ filter boardHasBingo updatedBoards) * draw
        else solveA (nextDraws, updatedBoards)

-- solve the problem (part B)
solveB :: Input -> Int
solveB (draws, boards) =
  let draw : nextDraws = draws
      updatedBoards = map (mark draw) boards
      nonBingoBoards = filter (not . boardHasBingo) updatedBoards
   in if null nonBingoBoards
        then sumOfUnmarked (head updatedBoards) * draw
        else solveB (nextDraws, nonBingoBoards)