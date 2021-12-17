module Day11 where

import qualified Data.Set as Set

-- type definitions
-- position: (x, y)
type Position = (Int, Int)

-- parsed input type
type Input = [[Int]]

-- parse string input into input type
parse :: String -> Input
parse = map (map (read . (: []))) . lines

getNeighbors :: Position -> [Position]
getNeighbors (x, y) =
  [ (x - 1, y),
    (x - 1, y - 1),
    (x - 1, y + 1),
    (x + 1, y),
    (x + 1, y -1),
    (x + 1, y + 1),
    (x, y - 1),
    (x, y + 1)
  ]

getSize :: Input -> (Int, Int)
getSize input = (width, height)
  where
    width = length (head input)
    height = length input

inBounds :: Input -> Position -> Bool
inBounds input (x, y) = x >= 0 && x < width && y >= 0 && y < height
  where
    (width, height) = getSize input

listModAt :: Int -> (a -> a) -> [a] -> [a]
listModAt n f xs = take n xs ++ [f (xs !! n)] ++ drop (n + 1) xs

modifyAt :: (Int -> Int) -> Input -> Position -> Input
modifyAt f i (x, y) = listModAt y (listModAt x f) i

getAt :: Position -> Input -> Int
getAt (x, y) input = input !! y !! x

flash :: Input -> Set.Set Position -> Position -> Input
flash input hasFlashed pos =
  let inBounds' = inBounds input
      neighbors = Set.fromList $ filter inBounds' $ getNeighbors pos
      unflashedNeighbors = Set.difference neighbors hasFlashed
      afterIncrementingNeighbors = foldl (modifyAt succ) input $ Set.toList unflashedNeighbors
   in modifyAt (const 0) afterIncrementingNeighbors pos

allPositions :: Input -> [Position]
allPositions input = [(x, y) | x <- [0 .. width - 1], y <- [0 .. height - 1]]
  where
    (width, height) = getSize input

-- step: take the current state, return the next state and the
-- number of flashes
step :: Input -> (Input, Int)
step input = step' (map (map succ) input) Set.empty
  where
    step' :: Input -> Set.Set Position -> (Input, Int)
    step' input flashedSoFar =
      let flashablePositions = filter (\p -> getAt p input > 9) $ allPositions input
       in if null flashablePositions
            then (input, Set.size flashedSoFar)
            else
              let flashPosition = head flashablePositions
                  nextInput = flash input flashedSoFar flashPosition
               in step' nextInput (Set.insert flashPosition flashedSoFar)

--

-- do n steps, and return the total number of flashes
doNSteps :: Int -> Input -> Int
doNSteps = doNSteps' 0
  where
    doNSteps' :: Int -> Int -> Input -> Int
    doNSteps' flashes 0 _ = flashes
    doNSteps' flashes n input =
      let (nextInput, stepFlashes) = step input
       in doNSteps' (flashes + stepFlashes) (n - 1) nextInput

-- return the first step in which all octopuses flash
getSynchronizedFlashStep :: Input -> Int
getSynchronizedFlashStep input = go' 1 input
  where
    (width, height) = getSize input
    cells = width * height
    go' :: Int -> Input -> Int
    go' n input =
      let (nextInput, flashes) = step input
       in if flashes == cells
            then n
            else go' (n + 1) nextInput

-- solve the problem (part A)
solveA :: Input -> Int
solveA = doNSteps 100

--

-- solve the problem (part B)
solveB :: Input -> Int
solveB = getSynchronizedFlashStep