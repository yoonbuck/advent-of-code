module Day03 (parse, Input, solveA, solveB) where

import           Data.Char  (isNumber)
import           Data.Maybe (mapMaybe)
import           Lib

type Input = [Line]

data Line = Line { getSymbols     :: [Symbol]
                 , getPartNumbers :: [PartNumber]
                 } deriving (Show)
data Symbol = Symbol { getCharacter :: Char
                     , getPosition  :: Int
                     } deriving (Show)
data PartNumber = PartNumber { getPartNumber :: Int
                             , getSpan       :: (Int, Int)
                             } deriving (Show)

parseLine :: String -> Line
parseLine line = go indexedLine [] [] Nothing
  where
    indexedLine = ix line

    go :: [(Int, Char)] -> [Symbol] -> [PartNumber] -> Maybe (Int, Int, Int) -> Line
    go [] syms nums numAcc = Line syms (appendPart nums numAcc)
    go ((_, '.'):rest) syms nums numAcc = go rest syms (appendPart nums numAcc) Nothing
    go ((i, ch):rest) syms nums numAcc
      | isNumber ch = go rest syms nums (continuePart numAcc i (read [ch]))
      | otherwise = go rest (Symbol ch i : syms) (appendPart nums numAcc) Nothing

    appendPart :: [PartNumber] -> Maybe (Int, Int, Int) -> [PartNumber]
    appendPart nums Nothing            = nums
    appendPart nums (Just (val, s, e)) = PartNumber val (s, e) : nums

    continuePart :: Maybe (Int, Int, Int) -> Int -> Int -> Maybe (Int, Int, Int)
    continuePart (Just (val, s, _)) i v = Just (val * 10 + v, s, i)
    continuePart Nothing i v            = Just (v, i, i)

parse :: String -> [Line]
parse = map parseLine . lines

componentTrio :: (Line -> [a]) -> [Line] -> [([a], [a], [a])]
componentTrio f = tail . (\ss -> zip3 ([]:[]:ss) ([]:ss ++ [[]]) (ss ++ [[],[]])) . map f

filterToAdjacentSymbols :: ([PartNumber], ([Symbol], [Symbol], [Symbol])) -> [PartNumber]
filterToAdjacentSymbols (ns, (ss1, ss2, ss3)) = filter hasAdjacentSymbol ns
  where
    ss = ss1 ++ ss2 ++ ss3
    hasAdjacentSymbol :: PartNumber -> Bool
    hasAdjacentSymbol (PartNumber _ (s, e)) =
      any (\(Symbol _ p) -> p >= (s-1) && p <= (e+1)) ss

solveA :: Input -> Int
solveA ls =
  zip (map getPartNumbers ls) (componentTrio getSymbols ls)
  |> concatMap filterToAdjacentSymbols
  |> map getPartNumber
  |> sum

filterToGearRatios :: ([Symbol], ([PartNumber], [PartNumber], [PartNumber])) -> [Int]
filterToGearRatios (ss, (ns1, ns2, ns3)) = mapMaybe get2AdjacentPartNumbers gears
  where
    gears = filter ((== '*') . getCharacter) ss
    ns = ns1 ++ ns2 ++ ns3

    get2AdjacentPartNumbers :: Symbol -> Maybe Int
    get2AdjacentPartNumbers (Symbol _ p) =
      if length adjacentNumbers == 2
      then Just (getPartNumber (head adjacentNumbers) * getPartNumber (adjacentNumbers !! 1))
      else Nothing
      where
        adjacentNumbers = filter isAdjacent ns
        isAdjacent :: PartNumber -> Bool
        isAdjacent (PartNumber _ (s, e)) = p >= (s-1) && p <= (e+1)

solveB :: Input -> Int
solveB ls =
  zip (map getSymbols ls) (componentTrio getPartNumbers ls)
  |> concatMap filterToGearRatios
  |> sum
