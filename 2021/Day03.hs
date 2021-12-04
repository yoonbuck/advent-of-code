module Day03 where

-- type definitions
type BitString = [Bool]

parseBitString :: String -> BitString
parseBitString = map (== '1')

bitStringToInt :: BitString -> Int
bitStringToInt = foldl (\acc x -> acc * 2 + (if x then 1 else 0)) 0

-- parsed input type
type Input = [BitString]

-- parse string input into input type
parse :: String -> Input
parse = map parseBitString . lines

flipBitString :: BitString -> BitString
flipBitString = map not

-- number of 0s, number of 1s
type BitCount = (Int, Int)

addBitStringToCount :: [BitCount] -> BitString -> [BitCount]
addBitStringToCount = zipWith adjustCount

adjustCount :: BitCount -> Bool -> BitCount
adjustCount (a, b) bit = if bit then (a, b + 1) else (a + 1, b)

getBitStringCounts :: Int -> [BitString] -> [BitCount]
getBitStringCounts places = foldl addBitStringToCount $ replicate places (0, 0)

countsToBitString :: [BitCount] -> BitString
countsToBitString = map $ uncurry (<)

solveA :: Input -> Int
solveA input = gamma * epsilon
  where
    bitLength = length $ head input
    resultBitString = countsToBitString $ getBitStringCounts bitLength input
    gamma = bitStringToInt resultBitString
    epsilon = bitStringToInt $ flipBitString resultBitString

pareDownBitString :: (Int -> Int -> Bool) -> [BitString] -> BitString
pareDownBitString comparator strings = result
  where
    -- get all of the first bits
    firstBits = map head strings
    -- count how many of firstBits are true
    firstBitIsTrueCount = length $ filter id firstBits
    firstBitIsFalseCount = length strings - firstBitIsTrueCount
    -- get the target bit
    targetBit = firstBitIsTrueCount `comparator` firstBitIsFalseCount
    -- get the candidates
    candidates = filter ((== targetBit) . head) strings
    result = case length candidates of
      0 -> error "no candidates"
      1 -> head candidates
      _ -> targetBit : pareDownBitString comparator (map tail candidates)

solveB :: Input -> Int
solveB input = oxygenGeneratorRanking * co2ScrubberRanking
  where
    oxygenGeneratorRanking = bitStringToInt $ pareDownBitString (>=) input
    co2ScrubberRanking = bitStringToInt $ pareDownBitString (<) input