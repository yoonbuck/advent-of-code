module Day05 where

import Data.List.Extra (firstJust)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, fromMaybe)

-- type definitions

type Seeds = [Int]

-- |
--  note ranges are (start, end [exclusive]),
--  not (start, length)
type SeedRange = (Int, Int)

type Maps = [MappingGroup]

type MappingGroup = [(Int, Int, Int)]

type SingleMapping = (Int, Int, Int)

-- parsed input type
type Input = (Seeds, Maps)

-- parse string input into input type
parse :: String -> Input
parse input =
  let seedsStr : rest = splitOn "\n\n" input
      seeds = map read $ tail $ words seedsStr
      restMaps = map (tail . lines) rest
      maps = map (map parseSingleMapping) restMaps
   in (seeds, maps)

parseSingleMapping :: String -> SingleMapping
parseSingleMapping = (\[a, b, c] -> (a, b, c)) . map read . words

applyAllMappingGroups :: [MappingGroup] -> [SeedRange] -> [SeedRange]
applyAllMappingGroups [] is = is
applyAllMappingGroups (mapping : mappings) inputs =
  applyAllMappingGroups mappings $ concatMap (applyMappingGroup mapping) inputs

applyMappingGroup :: MappingGroup -> SeedRange -> [SeedRange]
applyMappingGroup [] input = [input] -- never got mapped, so it stays as is
applyMappingGroup (mapping : mappings) input =
  let (mappedValues, unmappedValues) = applySingleMapping mapping input
   in mappedValues ++ concatMap (applyMappingGroup mappings) unmappedValues

-- (destStart, sourceStart, rangeLen) -> (inStart, inEnd) -> (mapped outputs, unmapped outputs)
applySingleMapping :: SingleMapping -> SeedRange -> ([SeedRange], [SeedRange])
applySingleMapping (dS, sS, l) (inS, inE)
  -- fully outside of mapping range
  | inS >= sE || inE <= sS = ([], [(inS, inE)])
  -- fully inside of mapping range
  | inS >= sS && inE <= sE = ([(inS + dist, inE + dist)], [])
  -- partially in the mapping range
  | inS >= sS = ([(inS + dist, sE + dist)], [(sE, inE)])
  | inE <= sE = ([(sS + dist, inE + dist)], [(inS, sS)])
  -- overlapping on both ends
  | otherwise = ([(sS + dist, sE + dist)], [(inS, sS), (sE, inE)])
  where
    sE = sS + l
    dist = dS - sS

--

-- solve the problem (part A)
solveA :: Input -> Int
solveA (seeds, mappings) =
  let seedRanges = map (\a -> (a, a + 1)) seeds
   in minimum $ map fst $ applyAllMappingGroups mappings seedRanges

--

-- solve the problem (part B)
solveB :: Input -> Int
solveB (seeds, mappings) =
  let seedRanges = pairUp seeds
   in minimum $ map fst $ applyAllMappingGroups mappings seedRanges

pairUp :: [Int] -> [SeedRange]
pairUp [] = []
pairUp (x1 : x2 : xs) = (x1, x1 + x2) : pairUp xs