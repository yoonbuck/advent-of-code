module Lib where

import Data.List.Split (splitOn)

splitParagraphs :: String -> [String]
splitParagraphs = splitOn "\n\n"