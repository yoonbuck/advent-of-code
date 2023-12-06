module Lib where

import Data.List.Split (splitOn)

splitParagraphs :: String -> [String]
splitParagraphs = splitOn "\n\n"

-- Natural numbers (incl. 0)
nats :: [Int]
nats = [0 ..]

-- Infinite list generator from naturals
natg :: (Int -> a) -> [a]
natg = (`map` nats)

-- Index list (from 0)
ix :: [a] -> [(Int, a)]
ix = zip nats

(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) = flip (.)

(|>) :: a -> (a -> b) -> b
a |> f = f a

annotate :: (Functor f) => (t -> b) -> f t -> f (t, b)
annotate fn = fmap (\a -> (a, fn a))