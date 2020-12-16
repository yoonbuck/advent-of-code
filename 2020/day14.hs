{-# LANGUAGE LambdaCase #-}

import           Data.List.Split (splitOn)
import qualified Data.Map as Map
import           Data.Int (Int64)
import           Data.List (foldl')

type Input = [Instruction]

data Bit = One
         | Zero
  deriving (Eq, Show)

toBit :: (Integral i) => i -> Bit
toBit 0 = Zero
toBit 1 = One

fromBit :: (Integral i) => Bit -> i
fromBit Zero = 0
fromBit One = 1

toBits :: (Integral i) => i -> [Bit]
toBits 0 = repeat Zero
toBits n = let (q, r) = n `quotRem` 2
           in toBit r:toBits q

fromBits :: (Integral i) => [Bit] -> i
fromBits [] = 0
fromBits (b:bs) = fromBit b + 2 * fromBits bs

type Mask = [Maybe Bit]

data Instruction = Assign Int64 Int64
                 | SetMask Mask
  deriving (Eq, Show)

data State = State { mask :: Mask, memory :: Map.Map Int64 Int64 }
  deriving (Eq, Show)

toMask :: String -> Mask
toMask = reverse
  . map
    (\case
       '0' -> Just Zero
       '1' -> Just One
       'X' -> Nothing)

parseInstr :: String -> Instruction
parseInstr line = case take 2 line of
  "me" -> let [a, v] = splitOn "] = " $ drop 4 line
          in Assign (read a) (read v)
  "ma" -> SetMask $ toMask $ drop 7 line

parse :: String -> Input
parse = map parseInstr . lines

type Runner = State -> Instruction -> State

solve :: Runner -> Input -> Int64
solve runner = sum
  . Map.elems
  . memory
  . foldl' runner (State { mask = mempty, memory = Map.empty })

runA :: Runner
runA (State _ mem) (SetMask newMask) = State newMask mem
runA (State msk mem) (Assign addr val) = State msk
  $ Map.insert addr (maskVal msk val) mem

maskVal :: Mask -> Int64 -> Int64
maskVal msk = fromBits . zipWith combineBitsA msk . toBits

combineBitsA :: Maybe Bit -> Bit -> Bit
combineBitsA (Just a) _ = a
combineBitsA Nothing b = b

runB :: Runner
runB (State _ mem) (SetMask newMask) = State newMask mem
runB (State msk mem) (Assign addr val) =
  State msk $ foldr (`Map.insert` val) mem $ floatMask msk addr

floatMask :: Mask -> Int64 -> [Int64]
floatMask msk = fromFloatingBits . zipWith combineBitsB msk . toBits

fromFloatingBits :: [Maybe Bit] -> [Int64]
fromFloatingBits [] = [0]
fromFloatingBits (Just b:bs) = map
  (\v -> (if b == One
          then 1
          else 0)
   + (2 * v))
  $ fromFloatingBits bs
fromFloatingBits (Nothing:bs) = fromFloatingBits (Just Zero:bs)
  ++ fromFloatingBits (Just One:bs)

combineBitsB :: Maybe Bit -> Bit -> Maybe Bit
combineBitsB Nothing _ = Nothing
combineBitsB (Just Zero) b = Just b
combineBitsB _ _ = Just One

main :: IO ()
main = do
  input <- getContents
  let parsed = parse input
  putStrLn $ "Part A: " ++ (show . solve runA) parsed
  putStrLn $ "Part B: " ++ (show . solve runB) parsed
