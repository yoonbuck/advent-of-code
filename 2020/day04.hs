import           Data.Char (isDigit)
import           Data.List.Split (splitOn)
import           Data.List.Extra (takeEnd, dropEnd)

{-|
  given a length, lower bound, and upper bound, check whether a string
  is of the correct length, all characters are numbers, and the parsed
  value is within the given bounds (inclusive)
-}
validateNum :: Int -> Int -> Int -> String -> Bool
validateNum len l u n = length n == len
  && all isDigit n
  && let n' = read n
     in n' >= l && n' <= u

{-|
  Required fields, along with rules for validating them
-}
validateRules :: [(String, String -> Bool)]
validateRules =
  [ ("byr", validateNum 4 1920 2002)
  , ("iyr", validateNum 4 2010 2020)
  , ("eyr", validateNum 4 2020 2030)
  , ( "hgt"
    , \s -> length s > 2
      && let n = dropEnd 2 s
         in case takeEnd 2 s of
              "cm" -> validateNum 3 150 193 n
              "in" -> validateNum 2 59 76 n
              _    -> False)
  , ( "hcl"
    , \s -> let (p:ps) = s
            in p == '#' && length ps == 6 && all (`elem` "0123456789abcdef") ps)
  , ("ecl", \s -> s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
  , ("pid", validateNum 9 0 999999999)]

{-|
  just the required fields
-}
required :: [String]
required = map fst validateRules

{-|
  determine whether a passport string has all of the @required@ fields
-}
hasAll :: String -> Bool
hasAll str = all (`elem` (map (head . splitOn ":") $ words str)) required

{-|
  determine whether a passport string has all of the required fields,
  and each of those fields are valid
-}
matchAll :: String -> Bool
matchAll = checkAll validateRules . map (splitOn ":") . words

checkAll :: [(String, String -> Bool)] -> [[String]] -> Bool
checkAll [] _ = True
checkAll (req:reqs) fields = checkOne req fields && checkAll reqs fields

checkOne :: (String, String -> Bool) -> [[String]] -> Bool
checkOne _ [] = False
checkOne r@(a, validator) ([k, v]:ls) =
  if a == k
  then validator v
  else checkOne r ls

main :: IO ()
main = do
  input <- getContents
  let parsed = splitOn "\n\n" input
  putStrLn $ "Part A: " ++ show (length $ filter hasAll parsed)
  putStrLn $ "Part B: " ++ show (length $ filter matchAll parsed)
