solveA :: [Integer] -> Integer
solveA xs = head [a * b | a <- xs, b <- xs, a + b == 2020]

solveB :: [Integer] -> Integer
solveB xs = head [a * b * c | a <- xs, b <- xs, c <- xs, a + b + c == 2020]

main :: IO ()
main = interact $ show . (\x -> (solveA x, solveB x)) . map read . lines
