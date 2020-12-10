import           Data.List (sort, group)

parse :: String -> [Int]
parse = diff . (\l -> 0:l ++ [maximum l + 3]) . sort . map read . lines

diff :: [Int] -> [Int]
diff i = zipWith (-) (tail i) i

solveA :: [Int] -> Int
solveA i = (1 + count 3 i) * count 1 i

count :: (Eq a) => a -> [a] -> Int
count d = length . filter (== d)

solveB :: [Int] -> Int
solveB = product . map ((factors !!) . length) . filter ((== 1) . head) . group

factors :: [Int]
factors = [1, 1, 2, 4, 7, 13, 21]

main :: IO ()
main = do
  input <- getContents
  let parsed = parse input
  putStrLn $ "Part A: " ++ show (solveA parsed)
  putStrLn $ "Part B: " ++ show (solveB parsed)
