module Day02 where

data Direction = Forward | Down | Up deriving (Show, Eq)

parseDirection :: String -> Direction
parseDirection "forward" = Forward
parseDirection "down" = Down
parseDirection "up" = Up
parseDirection x = error $ "Unknown direction: " ++ x

data Instruction = Instruction Direction Int
  deriving (Show, Eq)

-- split on space, then parse the direction and distance
parseInstruction :: String -> Instruction
parseInstruction str = Instruction (parseDirection dir) (read dist)
  where
    (dir, dist) = break (== ' ') str

-- horizontal posistion, depth
type Position = (Int, Int)

performInstruction :: Position -> Instruction -> Position
performInstruction (x, y) (Instruction Forward n) = (x + n, y)
performInstruction (x, y) (Instruction Down n) = (x, y + n)
performInstruction (x, y) (Instruction Up n) = (x, y - n)

-- multiply the horizontal position by the depth
getResult :: Position -> Int
getResult (x, y) = x * y

parse :: String -> [Instruction]
parse = map parseInstruction . lines

solveA :: [Instruction] -> Int
solveA = getResult . foldl performInstruction (0, 0)

-- Part 2 state: Horizontal position, depth, "aim"
type StateB = (Int, Int, Int)

performInstructionB :: StateB -> Instruction -> StateB
performInstructionB (pos, depth, aim) (Instruction Down n) = (pos, depth, aim + n)
performInstructionB (pos, depth, aim) (Instruction Up n) = (pos, depth, aim - n)
performInstructionB (pos, depth, aim) (Instruction Forward n) = (pos + n, depth + (aim * n), aim)

-- discard aim
stateBToPosition :: StateB -> Position
stateBToPosition (pos, depth, _) = (pos, depth)

solveB :: [Instruction] -> Int
solveB = getResult . stateBToPosition . foldl performInstructionB (0, 0, 0)

-- append a newline
appl :: String -> String
appl = (++ "\n")

main = interact $ appl . show . solveB . parse