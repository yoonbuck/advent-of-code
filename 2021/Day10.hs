module Day10 where

import Data.Either (lefts, rights)
import Data.List (sort)
import Data.List.Split (splitOn)

-- type definitions

-- parsed input type
type Input = [BracketString]

data BracketType = Round | Square | Curly | Angle deriving (Eq, Show)

data Direction = Opening | Closing deriving (Eq, Show)

data Bracket = Bracket Direction BracketType deriving (Eq, Show)

bracketByCharacter :: Char -> Bracket
bracketByCharacter '(' = Bracket Opening Round
bracketByCharacter '[' = Bracket Opening Square
bracketByCharacter '{' = Bracket Opening Curly
bracketByCharacter '<' = Bracket Opening Angle
bracketByCharacter ')' = Bracket Closing Round
bracketByCharacter ']' = Bracket Closing Square
bracketByCharacter '}' = Bracket Closing Curly
bracketByCharacter '>' = Bracket Closing Angle
bracketByCharacter _ = error "invalid bracket type"

type BracketStack = [BracketType]

type BracketString = [Bracket]

-- parse string input into input type
parse :: String -> Input
parse = map (map bracketByCharacter) . lines

-- Left: syntax error
-- Right: missing closing bracket(s)
getLineResult :: BracketString -> Either Int Int
getLineResult = getScore' []
  where
    getScore' :: BracketStack -> BracketString -> Either Int Int
    -- end of line, and nothing on the stack = valid
    getScore' [] [] = error "correct input!"
    -- end of line, but something on the stack = invalid
    getScore' stack [] = Right $ scoreB stack
    getScore' stack ((Bracket Opening bracketType) : remainingInput) =
      getScore' (bracketType : stack) remainingInput
    getScore' (bracketTypeStack : restOfStack) ((Bracket Closing bracketTypeInput) : remainingInput) =
      if bracketTypeInput == bracketTypeStack
        then getScore' restOfStack remainingInput
        else Left $ scoreA bracketTypeInput
    getScore' [] ((Bracket Closing _) : _) = error "no bracket on the stack!"

scoreA :: BracketType -> Int
scoreA Round = 3
scoreA Square = 57
scoreA Curly = 1197
scoreA Angle = 25137

scoreB :: BracketStack -> Int
scoreB = foldl (\score char -> score * 5 + valueOf char) 0

valueOf :: BracketType -> Int
valueOf Round = 1
valueOf Square = 2
valueOf Curly = 3
valueOf Angle = 4

-- solve the problem (part A)
solveA :: Input -> Int
solveA = sum . lefts . map getLineResult

-- solve the problem (part B)
solveB :: Input -> Int
solveB = median . rights . map getLineResult

median :: [Int] -> Int
median list = sort list !! middleIndex
  where
    middleIndex = length list `div` 2
