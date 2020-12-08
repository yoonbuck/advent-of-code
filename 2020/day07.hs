import           Data.List.Split (splitOn)
import           Data.List.Extra (dropEnd)
import           Data.Bifunctor (second)
import qualified Data.Map as Map

type Input = [Rule]

type Bag = String

type Rule = (Bag, [(Int, Bag)])

parse :: String -> Input
parse = map parseRule . lines

parseRule :: String -> Rule
parseRule str =
  let [name, contents] = splitOn " contain " str
  in ( parseBag name
     , if contents == "no other bags."
       then []
       else map
         (\c -> let (num:bag) = words c
                in (read num, parseBag $ unwords bag))
         $ splitOn ", " contents)

parseBag :: String -> Bag
parseBag = unwords . dropEnd 1 . words

leadsTo :: Bag -> Map.Map Bag [Bag] -> Bag -> Bool
leadsTo target ruleMap from
  | from == target = True
  | otherwise = case ruleMap Map.!? from of
    Nothing      -> False
    Just results -> any (leadsTo target ruleMap) results

countBags :: Map.Map Bag [(Int, Bag)] -> Bag -> Int
countBags ruleMap bagType = case ruleMap Map.!? bagType of
  Nothing      -> 0
  Just results -> succ
    $ sum
    $ map (\(count, bag) -> count * countBags ruleMap bag) results

solveA :: Input -> Int
solveA input =
  let ruleMap = Map.fromList $ map (second (map snd)) input
  in pred $ length $ filter (leadsTo "shiny gold" ruleMap) $ Map.keys ruleMap

solveB :: Input -> Int
solveB input = let ruleMap = Map.fromList input
               in pred $ countBags ruleMap "shiny gold"

main = do
  input <- getContents
  let parsed = parse input
  putStrLn $ "Part A: " ++ show (solveA parsed)
  putStrLn $ "Part B: " ++ show (solveB parsed)
