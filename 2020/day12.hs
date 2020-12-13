import           Data.List (foldl')

type Input = [(Char, Int)]

type Pos = (Int, Int)

type State = (Pos, Pos)

type Mover = State -> (Char, Int) -> State

parse :: String -> Input
parse = map (\(n:s) -> (n, read s)) . lines

solve :: Mover -> State -> Input -> Int
solve mover initial = manhattan . fst . foldl' mover initial

rot :: Pos -> Int -> Pos
rot d 0 = d
rot (dx, dy) 90 = (-dy, dx)
rot d v = rot (rot d (v - 90)) 90

manhattan :: Pos -> Int
manhattan (a, b) = abs a + abs b

moveA :: State -> (Char, Int) -> State
moveA (p@(x, y), d@(dx, dy)) (inst, val)
  | inst == 'N' = ((x, y + val), d)
  | inst == 'E' = ((x + val, y), d)
  | inst == 'S' = ((x, y - val), d)
  | inst == 'W' = ((x - val, y), d)
  | inst == 'L' = (p, rot d val)
  | inst == 'R' = (p, rot d (360 - val))
  | inst == 'F' = ((x + val * dx, y + val * dy), d)

moveB :: State -> (Char, Int) -> State
moveB (s@(x, y), w@(wx, wy)) (inst, val)
  | inst == 'N' = (s, (wx, wy + val))
  | inst == 'E' = (s, (wx + val, wy))
  | inst == 'S' = (s, (wx, wy - val))
  | inst == 'W' = (s, (wx - val, wy))
  | inst == 'L' = (s, rot w val)
  | inst == 'R' = (s, rot w (360 - val))
  | inst == 'F' = ((x + wx * val, y + wy * val), w)

main :: IO ()
main = do
  input <- getContents
  let parsed = parse input
  putStrLn $ "Part A: " ++ (show . solve moveA ((0, 0), (1, 0))) parsed
  putStrLn $ "Part B: " ++ (show . solve moveB ((0, 0), (10, 1))) parsed
