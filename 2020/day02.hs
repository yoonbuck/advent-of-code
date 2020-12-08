import           Data.Tuple.Extra (both)

type Rule = (Char, Int, Int)

count :: (Eq a) => a -> [a] -> Int
count _ [] = 0
count a (x:xs) = count a xs
  + if a == x
    then 1
    else 0

validateA :: (Rule, String) -> Bool
validateA ((c, l, u), str) = let num = count c str
                             in num >= l && num <= u

validateB :: (Rule, String) -> Bool
validateB ((c, l, u), str) = (str !!! l == c) /= (str !!! u == c)

(!!!) :: String -> Int -> Char
xs !!! n
  | n > length xs = '\0'
  | otherwise = xs !! (n - 1)

parse :: String -> (Rule, String)
parse input = ((char, l, u), pass)
  where
    [left, _:pass] = splitBy ':' input

    [bounds, [char]] = splitBy ' ' left

    [l', u'] = splitBy '-' bounds

    l = read l'

    u = read u'

main :: IO ()
main = interact
  $ show
  . both (length . filter id)
  . (\x -> (map validateA x, map validateB x))
  . map parse
  . lines

{-|
  Splits a given string at a specific delimeter.

  e.g., @splitBy ','@ to split at commas
-}
splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy _ [] = [[]]
splitBy delim (c:cs)
  | c == delim = []:splitBy delim cs
  | otherwise = case splitBy delim cs of
    []      -> [[c]]
    (cs':r) -> (c:cs'):r
