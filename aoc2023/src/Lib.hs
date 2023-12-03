module Lib (splitParagraphs) where

import           Data.List.Split (splitOn)

splitParagraphs :: String -> [String]
splitParagraphs = splitOn "\n\n"
