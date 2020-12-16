import qualified Data.Map as Map
import           Data.Maybe (fromJust)

-- (index, map of last seen indices, next value)
type State = (Int, Map.Map Int Int, Int)

input :: [Int]
input = [14, 3, 1, 0, 9, 5]

solve :: Int -> Int
solve num = run
  (num - length input)
  (length input, Map.fromList $ zip input [1 ..], last input)

run :: Int -> State -> Int
run count l@(_, _, x)
  | count == 0 = x
  | otherwise = run (count - 1) $ next l

next :: State -> State
next (i, map, x) = ( 1 + i
                   , Map.insert x i map
                   , if Map.member x map
                     then i - fromJust (Map.lookup x map)
                     else 0)

main :: IO ()
main = do
  putStrLn $ "Part A: " ++ show (solve 2020)
  putStrLn $ "Part B: " ++ show (solve 30000000)
