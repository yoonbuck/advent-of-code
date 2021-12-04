import Data.List.Split (splitOn)

splitParagraphs :: String -> [String]
splitParagraphs = splitOn "\n\n"

-- type definitions

-- parsed input type
type Input = [Int]

-- parse string input into input type
parse :: String -> Input
parse = map read . lines

--

-- solve the problem (part A)
solveA :: Input -> Int
solveA = undefined

--

-- solve the problem (part B)
solveB :: Input -> Int
solveB = undefined