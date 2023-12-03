module Lib (splitParagraphs, nats, natg, ix, (>.>)) where

import           Data.List.Split (splitOn)

splitParagraphs :: String -> [String]
splitParagraphs = splitOn "\n\n"

-- Natural numbers (incl. 0)
nats :: [Int]
nats = iterate succ 0

-- Infinite list generator from naturals
natg :: (Int -> a) -> [a]
natg = (`map` nats)

-- Index list (from 0)
ix :: [a] -> [(Int, a)]
ix = zip nats

(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) = flip (.)
