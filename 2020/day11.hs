-- I should probably use Arrays instead of Lists!
-- There's way to much O(n) List lookup happening
-- here, so this solution is pretty slow!
solve :: [[Char]] -> Ticker -> String
solve input ticker = let (changed, new) = tick ticker input
                     in if changed
                        then solve new ticker
                        else show $ sum $ map (count '#') new

count :: (Eq a) => a -> [a] -> Int
count d = length . filter (== d)

tick :: Ticker -> [[Char]] -> (Bool, [[Char]])
tick ticker board =
  let (w, h) = (length $ head board, length board)
  in (\newBoard -> (newBoard /= board, newBoard))
     $ [[ticker board (x, y) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]

type Ticker = [[Char]] -> (Int, Int) -> Char

tickA :: Ticker
tickA originalBoard (x, y) = case originalBoard !! y !! x of
  '.' -> '.'
  'L' -> if countNeighbors (x, y) originalBoard == 0
         then '#'
         else 'L'
  '#' -> if countNeighbors (x, y) originalBoard >= 4
         then 'L'
         else '#'

tickB :: Ticker
tickB originalBoard (x, y) = case originalBoard !! y !! x of
  '.' -> '.'
  'L' -> if countRaycast (x, y) originalBoard == 0
         then '#'
         else 'L'
  '#' -> if countRaycast (x, y) originalBoard >= 5
         then 'L'
         else '#'

countNeighbors :: (Int, Int) -> [[Char]] -> Int
countNeighbors (x, y) board = count
  '#'
  [boardAt (x', y') board
  | x' <- [x - 1 .. x + 1]
  , y' <- [y - 1 .. y + 1]
  , not (x' == x && y' == y)]

boardAt :: (Int, Int) -> [[Char]] -> Char
boardAt (x, y) board =
  if y < 0 || y >= length board
  then ' '
  else let row = board !! y
       in if x < 0 || x >= length row
          then ' '
          else row !! x

countRaycast :: (Int, Int) -> [[Char]] -> Int
countRaycast (x, y) board = count
  '#'
  [raycast (x + dx, y + dy) (dx, dy) board
  | dx <- [-1 .. 1]
  , dy <- [-1 .. 1]
  , not (dx == 0 && dy == 0)]

raycast :: (Int, Int) -> (Int, Int) -> [[Char]] -> Char
raycast (x, y) (dx, dy) board = case boardAt (x, y) board of
  '.' -> raycast (x + dx, y + dy) (dx, dy) board
  a   -> a

main :: IO ()
main = do
  input <- getContents
  let parsed = lines input
  putStrLn $ "Part A: " ++ solve parsed tickA
  putStrLn $ "Part B: " ++ solve parsed tickB
