import           Data.List.Split (splitOn)

type Input = [String]

parse :: String -> Input
parse = splitOn "\n\n"

solveA :: Input -> String
solveA = const "Not yet implemented"

solveB :: Input -> String
solveB = const "Not yet implemented"

main :: IO ()
main = do
  input <- getContents
  let parsed = parse input
  putStrLn $ "Part A: " ++ show (solveA parsed)
  putStrLn $ "Part B: " ++ show (solveB parsed)
