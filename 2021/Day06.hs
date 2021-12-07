module Day06 where

import Data.Function ((&))
import Data.List.Split (splitOn)
import qualified Data.Map as Map

-- parsed input type
type Input = [Int]

type School = Map.Map Int Int

addToSchool :: School -> Int -> School
addToSchool school n = Map.insertWith (+) n 1 school

buildSchool :: [Int] -> School
buildSchool = foldl addToSchool Map.empty

ageSchool :: School -> School
ageSchool school =
  let agedList = Map.mapKeys (\x -> x - 1) school
      agedOutFishCount = Map.findWithDefault 0 (-1) agedList
   in agedList
        & Map.delete (-1)
        & Map.insert 8 agedOutFishCount
        & Map.insertWith (+) 6 agedOutFishCount

ageSchoolNTimes :: Int -> School -> School
ageSchoolNTimes n school = iterate ageSchool school !! n

countFish :: School -> Int
countFish = Map.foldl (+) 0

-- parse string input into input type
parse :: String -> Input
parse = map read . splitOn ","

-- solve the problem (part A)
solveA :: Input -> Int
solveA = countFish . ageSchoolNTimes 80 . buildSchool

-- solve the problem (part B)
solveB :: Input -> Int
solveB = countFish . ageSchoolNTimes 256 . buildSchool
