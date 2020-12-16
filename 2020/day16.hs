{-# LANGUAGE TupleSections #-}

import           Data.List.Split (splitOn)
import qualified Data.Set as Set
import           Data.Bifunctor (first)
import           Data.List (foldl')

type Bound = (Int, Int)

type Field = (String, [Bound])

type Ticket = [Int]

type Input = ([Field], Ticket, [Ticket])

inBound :: Int -> Bound -> Bool
inBound v (l, u) = l <= v && v <= u

validForField :: Int -> Field -> Bool
validForField v (_, bs) = any (inBound v) bs

parse :: String -> Input
parse input = let [fields, yourTicket, nearbyTickets] = splitOn "\n\n" input
              in ( parseFields fields
                 , head $ parseTickets yourTicket
                 , parseTickets nearbyTickets)

parseFields :: [Char] -> [Field]
parseFields = map ((\[n, c] -> (n, parseBounds c)) . splitOn ": ") . lines

parseBounds :: String -> [Bound]
parseBounds = map ((\[a, b] -> (a, b)) . map read . splitOn "-")
  . splitOn " or "

parseTickets :: String -> [Ticket]
parseTickets = map (map read . splitOn ",") . tail . lines

solveA :: Input -> Int
solveA (fields, _, tickets) = sum $ map (sum . getInvalids fields) tickets

isValidTicket :: [Field] -> Ticket -> Bool
isValidTicket fs = all (isValid fs)

getInvalids :: [Field] -> Ticket -> [Int]
getInvalids fs = filter (not . isValid fs)

isValid :: [Field] -> Int -> Bool
isValid fs d = any (validForField d) fs

solveB :: Input -> Int
solveB (fields, myTicket, tickets) =
  let validTickets = filter (isValidTicket fields) tickets
      fieldPositions = getFieldPositions fields validTickets
  in product
     $ map ((myTicket !!) . fst)
     $ filter (\(_, (c1:c2:_, _)) -> c1 == 'd' && c2 == 'e')
     $ resolve fieldPositions

getFieldPositions :: [Field] -> [[Int]] -> [(Set.Set Int, Field)]
getFieldPositions fields tickets = discardPositions
  (map (Set.fromList [0 .. pred $ length $ head tickets], ) fields)
  tickets

-- this is all probably super duper inefficient :)
resolve :: [(Set.Set Int, Field)] -> [(Int, Field)]
resolve sets
  | all ((== 1) . Set.size . fst) sets = map (first $ head . Set.toList) sets
  | otherwise = resolve
    $ foldl' elimSingleAnswers sets [0 .. (pred $ length sets)]

elimSingleAnswers :: [(Set.Set Int, Field)] -> Int -> [(Set.Set Int, Field)]
elimSingleAnswers setList i
  | (== 1) $ Set.size set = removeExcept setList i $ Set.findMin set
  | otherwise = setList
  where
    set = fst $ setList !! i

removeExcept :: [(Set.Set Int, Field)] -> Int -> Int -> [(Set.Set Int, Field)]
removeExcept setList i v = zipWith
  (\t idx -> first
     (if idx == i
      then id
      else Set.delete v)
     t)
  setList
  [0 ..]

discardPositions
  :: [(Set.Set Int, Field)] -> [Ticket] -> [(Set.Set Int, Field)]
discardPositions = foldl' (\as t -> map (checkField t) as)

checkField :: Ticket -> (Set.Set Int, Field) -> (Set.Set Int, Field)
checkField ticket (set, f) = (, f)
  $ foldr (\(_, idx) -> Set.delete idx) set
  $ filter (\(v, _) -> not $ validForField v f)
  $ zip ticket [0 ..]

main :: IO ()
main = do
  input <- getContents
  let parsed = parse input
  putStrLn $ "Part A: " ++ show (solveA parsed)
  putStrLn $ "Part B: " ++ show (solveB parsed)
