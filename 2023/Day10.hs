module Day10 where

import Data.Foldable (find)
import Data.List (transpose)
import Data.Maybe (fromJust)
import GHC.Arr qualified as A
import GHC.Base (divInt)

-- type definitions

data Tile
  = Ground
  | Vertical
  | Horizontal
  | NEBend
  | NWBend
  | SWBend
  | SEBend
  | StartPoint
  deriving (Show, Eq)

data Direction
  = North
  | South
  | East
  | West
  deriving (Show, Eq)

parseTile :: Char -> Tile
parseTile '.' = Ground
parseTile '|' = Vertical
parseTile '-' = Horizontal
parseTile 'L' = NEBend
parseTile 'J' = NWBend
parseTile '7' = SWBend
parseTile 'F' = SEBend
parseTile 'S' = StartPoint

move :: Direction -> (Int, Int) -> (Int, Int)
move North (x, y) = (x, y - 1)
move South (x, y) = (x, y + 1)
move East (x, y) = (x + 1, y)
move West (x, y) = (x - 1, y)

-- parsed input type
type Input = A.Array (Int, Int) Tile

-- for debug
array :: (A.Ix i) => (i, i) -> [(i, e)] -> A.Array i e
array = A.array

tileAt :: Input -> (Int, Int) -> Tile
tileAt arr pos
  | A.inRange (A.bounds arr) pos = arr A.! pos
  | otherwise = Ground

replaceStart :: Input -> (Input, (Int, Int))
replaceStart arr =
  let pos = fst $ fromJust $ find ((== StartPoint) . snd) $ A.assocs arr
      at = tileAt arr
      tn = at $ move North pos
      te = at $ move East pos
      ts = at $ move South pos
      tw = at $ move West pos
      tnc = tn == Vertical || tn == SWBend || tn == SEBend
      tec = te == Horizontal || te == NWBend || te == SWBend
      tsc = ts == Vertical || ts == NEBend || ts == NWBend
      twc = tw == Horizontal || tw == NEBend || tw == SEBend
   in (arr A.// [(pos, getReplacement tnc tec tsc twc)], pos)

getReplacement ::
  Bool -> -- north connectivity
  Bool -> -- east connectivity
  Bool -> -- south connectivity
  Bool -> -- west connectivity
  Tile
getReplacement True True False False = NEBend
getReplacement False True True False = SEBend
getReplacement False False True True = SWBend
getReplacement True False False True = NWBend
getReplacement True False True False = Vertical
getReplacement False True False True = Horizontal
getReplacement a b c d = error $ "invalid start point - connectivity: " ++ show (a, b, c, d)

getContinuationDirection :: Direction -> Tile -> Direction
getContinuationDirection North Vertical = North
getContinuationDirection South Vertical = South
getContinuationDirection East Horizontal = East
getContinuationDirection West Horizontal = West
getContinuationDirection South NEBend = East
getContinuationDirection West NEBend = North
getContinuationDirection South NWBend = West
getContinuationDirection East NWBend = North
getContinuationDirection North SWBend = West
getContinuationDirection East SWBend = South
getContinuationDirection North SEBend = East
getContinuationDirection West SEBend = South
getContinuationDirection d t = error $ "not connected: " ++ show (d, t)

-- parse string input into input type
parse :: String -> Input
parse input =
  let arr = transpose $ map (map parseTile) $ lines input
      height = length $ head arr
      width = length arr
      arr' = concat arr
   in A.listArray ((0, 0), (width - 1, height - 1)) arr'

--

-- solve the problem (part A)
solveA :: Input -> Int
solveA input = go startPos startDir 0
  where
    (map, startPos) = replaceStart input
    at = tileAt map
    startDir = startDirection $ at startPos

    go :: (Int, Int) -> Direction -> Int -> Int
    go pos dir acc
      | pos == startPos && acc > 0 = acc `divInt` 2
      | otherwise =
          let tile = at pos
              dir' = getContinuationDirection dir tile
           in go (move dir' pos) dir' $ acc + 1

startDirection :: Tile -> Direction
startDirection Vertical = North
startDirection Horizontal = East
startDirection NEBend = South
startDirection NWBend = East
startDirection SWBend = North
startDirection SEBend = West
startDirection t = error $ "no start direction for " ++ show t

--

-- solve the problem (part B)
solveB :: Input -> Int
solveB = undefined
