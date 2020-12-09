type Input = [Int]

preambleLength :: Int
preambleLength = 25

parse :: String -> Input
parse = map read . lines

solveA :: Input -> Int
solveA nums@(_:next) = if hasSum $ splitAt preambleLength nums
                       then solveA next
                       else nums !! preambleLength

hasSum :: ([Int], [Int]) -> Bool
hasSum (ns, t:_) = not $ null [0 | a <- ns, b <- ns, a /= b, a + b == t]

findSubA :: Int -> [Int] -> Int
findSubA target list@(f:t) = case findSubB target (f, f) list of
  Just soln -> soln
  Nothing   -> findSubA target t

findSubB :: Int -> (Int, Int) -> [Int] -> Maybe Int
findSubB 0 (cMin, cMax) _ = Just $ cMin + cMax
findSubB _ (_, _) [] = Nothing
findSubB target (cMin, cMax) (n:ns)
  | n > target = Nothing
  | otherwise = findSubB (target - n) (min cMin n, max cMax n) ns

main :: IO ()
main = do
  input <- getContents
  let parsed = parse input
  let solnA = solveA parsed
  putStrLn $ "Part A: " ++ show solnA
  putStrLn $ "Part B: " ++ show (findSubA solnA parsed)
