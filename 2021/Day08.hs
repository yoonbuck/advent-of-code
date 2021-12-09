module Day08 where

import Data.Bool (bool)
import Data.List (permutations)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

splitParagraphs :: String -> [String]
splitParagraphs = splitOn "\n\n"

-- type definitions

type SignalCombination = [Bool]

parseSignalCombination :: String -> SignalCombination
parseSignalCombination signal = map (`elem` signal) ['a' .. 'g']

countWiresOn :: SignalCombination -> Int
countWiresOn = sum . map (bool 0 1)

type Entry = ([SignalCombination], [SignalCombination])

-- parsed input type
type Input = [Entry]

-- parse string input into input type
parse :: String -> Input
parse = map parseEntry . lines

parseEntry :: String -> Entry
parseEntry input =
  let [left, right] = splitOn " | " input
   in (parseCombinations left, parseCombinations right)
  where
    parseCombinations = map parseSignalCombination . words

transformInputA :: Input -> [SignalCombination]
transformInputA = concatMap snd

-- solve the problem (part A)
solveA :: Input -> Int
solveA = length . filter ((`elem` [2, 3, 4, 7]) . countWiresOn) . transformInputA

-- solve the problem (part B)
solveB :: Input -> Int
solveB = sum . map getOutputNumber

getOutputNumber :: Entry -> Int
getOutputNumber (left, right) =
  let mapping = solveMapping left
   in decimalize $ map (fromJust . toDigit . applyMapping mapping) right

-- turn a list of digits into a decimal number
decimalize :: [Int] -> Int
decimalize = foldl (\acc x -> acc * 10 + x) 0

-- seven segment display mapping
toDigit :: SignalCombination -> Maybe Int
toDigit [True, True, True, False, True, True, True] = Just 0
toDigit [False, False, True, False, False, True, False] = Just 1
toDigit [True, False, True, True, True, False, True] = Just 2
toDigit [True, False, True, True, False, True, True] = Just 3
toDigit [False, True, True, True, False, True, False] = Just 4
toDigit [True, True, False, True, False, True, True] = Just 5
toDigit [True, True, False, True, True, True, True] = Just 6
toDigit [True, False, True, False, False, True, False] = Just 7
toDigit [True, True, True, True, True, True, True] = Just 8
toDigit [True, True, True, True, False, True, True] = Just 9
toDigit _ = Nothing

isDigit :: SignalCombination -> Bool
isDigit combo = case toDigit combo of Just _ -> True; Nothing -> False

-- mapping is a tuple of seven ints
type Mapping = [Int]

applyMapping :: Mapping -> SignalCombination -> SignalCombination
applyMapping mapping combo = map (combo !!) mapping

isCorrectMapping :: Mapping -> [SignalCombination] -> Bool
isCorrectMapping mapping = all (isDigit . applyMapping mapping)

allPossibleMappings :: [Mapping]
allPossibleMappings = permutations [0 .. 6]

-- for each possible mapping, find one that is correct
solveMapping :: [SignalCombination] -> Mapping
solveMapping combo = head $ filter (`isCorrectMapping` combo) allPossibleMappings
