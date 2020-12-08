import           Data.List (sort, foldl')

toSeat :: [Char] -> Int
toSeat = bin2int . map rep

rep :: Num p => Char -> p
rep 'F' = 0
rep 'L' = 0
rep 'R' = 1
rep 'B' = 1

bin2int :: [Int] -> Int
bin2int = foldl' (\a b -> 2 * a + b) 0

findGap :: (Eq p, Num p) => [p] -> p
findGap (a:bs@(b:_))
  | b - a /= 1 = b - 1
  | otherwise = findGap bs

solveA :: [Int] -> Int
solveA = maximum

solveB :: [Int] -> Int
solveB = findGap . sort

main :: IO ()
main = interact $ show . (\x -> (solveA x, solveB x)) . map toSeat . lines
