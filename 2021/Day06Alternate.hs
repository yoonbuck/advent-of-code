{-
 it struck me that a map from natural numbers to something (particularly when
 those numbers are relatively low) could also be called a "list" :)

 so I thought I should try to do it that way too! this feels like a really
 inefficient situation with the traversals in addToSchool, so I'm sure there's
 a better (and more haskelly) way to do it.

 just for fun, I'm ignoring the empty list case and operating on an infinite
 list, because it's haskell and I can!

 I have no idea how the performance compares between the two versions :)
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day06Alternate where

import Data.Function ((&))
import Data.List.Split (splitOn)
import qualified Data.Map as Map

-- parsed input type
type Input = [Int]

type School = [Int]

addToSchool :: Int -> Int -> School -> School
addToSchool count age (fish0 : rest)
  | age == 0 = (fish0 + count) : rest
  | otherwise = fish0 : addToSchool count (age - 1) rest

buildSchool :: [Int] -> School
buildSchool = foldr (addToSchool 1) $ repeat 0

ageSchool :: School -> School
ageSchool (agingOutFish : rest) =
  rest
    & addToSchool agingOutFish 6
    & addToSchool agingOutFish 8

ageSchoolNTimes :: Int -> School -> School
ageSchoolNTimes n school = iterate ageSchool school !! n

countFish :: School -> Int
countFish = sum . take 9

-- parse string input into input type
parse :: String -> Input
parse = map read . splitOn ","

-- solve the problem (part A)
solveA :: Input -> Int
solveA = countFish . ageSchoolNTimes 80 . buildSchool

-- solve the problem (part B)
solveB :: Input -> Int
solveB = countFish . ageSchoolNTimes 256 . buildSchool
