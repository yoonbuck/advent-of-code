module Day04 where

import Data.List (intersect)
import Data.List.Split (splitOn)
import Data.Tuple.Extra (both)

-- type definitions
type Scratchcard = ([Int], [Int])

-- parsed input type
type Input = [Scratchcard]

-- parse string input into input type
parse :: String -> Input
parse = map parseCard . lines

parseCard :: String -> Scratchcard
parseCard line =
  let [_, numbers] = splitOn ": " line
      [l, r] = splitOn " | " numbers
   in both (map read . words) (l, r)

-- solve the problem (part A)
solveA :: Input -> Int
solveA = sum . map (toScore . toCounts)

toCounts :: Scratchcard -> Int
toCounts = length . uncurry intersect

toScore :: Int -> Int
toScore 0 = 0
toScore n = 2 ^ (n - 1)

-- solve the problem (part B)
solveB :: Input -> Int
solveB = sum . map fst . accumulate . map ((1,) . toCounts)

accumulate :: [(Int, Int)] -> [(Int, Int)]
accumulate [] = []
accumulate (card@(count, score) : cards) = card : accumulate (bump score count cards)

bump :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
bump _ _ [] = []
bump 0 _ cards = cards
bump n count ((cardCount, cardScore) : cards) = (cardCount + count, cardScore) : bump (pred n) count cards
