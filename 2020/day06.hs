import           Data.Char (isLetter)
import qualified Data.Set as Set
import           Data.List.Split (splitOn)

partA :: [String] -> Int
partA = sum . map (Set.size . Set.fromList . filter isLetter)

partB :: [String] -> Int
partB = sum
  . map
    (Set.size
     . foldr1 Set.intersection
     . map (Set.fromList . filter isLetter)
     . lines)

main :: IO ()
main = do
  input <- getContents
  let parsed = splitOn "\n\n" input
  print $ partA parsed
  print $ partB parsed
