countTrees :: Int -> Int -> [String] -> Int
countTrees c r l = countTrees' c r l 0

countTrees' :: Int -> Int -> [String] -> Int -> Int
countTrees' _ _ [] _ = 0
countTrees' c r rows@(row:_) pos =
  countTrees' c r (skip r rows) ((pos + c) `mod` length row)
  + if (row !! pos) == '#'
    then 1
    else 0

skip :: Int -> [a] -> [a]
skip 0 ls = ls
skip _ [] = []
skip n (_:ds) = skip (n - 1) ds

part2 :: [(Int, Int)]
part2 = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

main :: IO ()
main = do
  input <- getContents
  let parsed = lines input
  putStrLn $ "Part A: " ++ show (countTrees 3 1 parsed)
  putStrLn
    $ "Part B: "
    ++ show (product $ map (\(c, r) -> countTrees c r parsed) part2)
