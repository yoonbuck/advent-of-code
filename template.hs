import           Data.List.Split (splitOn)

type Input = [String]

type Output = String

parse :: String -> Input
parse = splitOn "\n\n"

solveA :: Input -> Output
solveA = const "Not yet implemented"

solveB :: Input -> Output
solveB = const "Not yet implemented"

main :: IO ()
main = do
  input <- getContents
  let parsed = parse input
  print $ solveA parsed
  print $ solveB parsed