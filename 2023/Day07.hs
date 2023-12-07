module Day07 (Input, parse, solveA, solveB) where

import Data.Foldable (find)
import Data.List (sortOn)
import Data.Ord qualified
import Data.Tuple.Extra (first)
import Lib ((>.>), (|>))
import Test.LeanCheck.Stats (counts)

-- type definitions

data CardLabel
  = C2
  | C3
  | C4
  | C5
  | C6
  | C7
  | C8
  | C9
  | CT
  | CJ
  | CQ
  | CK
  | CA
  deriving (Show, Ord, Eq)

data CardLabelB
  = BJ
  | B2
  | B3
  | B4
  | B5
  | B6
  | B7
  | B8
  | B9
  | BT
  | BQ
  | BK
  | BA
  deriving (Show, Ord, Eq)

parseCard :: Char -> CardLabel
parseCard '2' = C2
parseCard '3' = C3
parseCard '4' = C4
parseCard '5' = C5
parseCard '6' = C6
parseCard '7' = C7
parseCard '8' = C8
parseCard '9' = C9
parseCard 'T' = CT
parseCard 'J' = CJ
parseCard 'Q' = CQ
parseCard 'K' = CK
parseCard 'A' = CA

toCardB :: CardLabel -> CardLabelB
toCardB C2 = B2
toCardB C3 = B3
toCardB C4 = B4
toCardB C5 = B5
toCardB C6 = B6
toCardB C7 = B7
toCardB C8 = B8
toCardB C9 = B9
toCardB CT = BT
toCardB CJ = BJ
toCardB CQ = BQ
toCardB CK = BK
toCardB CA = BA

type Hand = [CardLabel]

type Play = (Hand, Int)

data Strength
  = HighCard -- CardLabel CardLabel CardLabel CardLabel CardLabel
  | OnePair -- CardLabel CardLabel CardLabel CardLabel
  | TwoPair -- CardLabel CardLabel CardLabel
  | ThreeOfAKind -- CardLabel CardLabel
  | FullHouse -- CardLabel CardLabel
  | FourOfAKind -- CardLabel
  | FiveOfAKind -- CardLabel
  deriving (Show, Ord, Eq)

-- parsed input type
type Input = [Play]

-- parse string input into input type
parse :: String -> Input
parse = map parsePlay . lines

parsePlay :: String -> Play
parsePlay s =
  let [cards, bet] = words s
   in (map parseCard cards, read bet)

scoreHand :: (Eq a) => [a] -> Strength
scoreHand cards
  | head cardCounts == 5 = FiveOfAKind
  | head cardCounts == 4 = FourOfAKind
  | head cardCounts == 3 && cardCounts !! 1 == 2 = FullHouse
  | head cardCounts == 3 = ThreeOfAKind
  | head cardCounts == 2 && cardCounts !! 1 == 2 = TwoPair
  | head cardCounts == 2 = OnePair
  | otherwise = HighCard
  where
    cardCounts = sortOn Data.Ord.Down (map snd $ counts cards)

-- solve the problem (part A)
solveA :: Input -> Int
solveA =
  map (\(hand, bet) -> (scoreHand hand, hand, bet))
    >.> sortOn (\(strength, hand, _) -> (strength, hand))
    >.> zipWith (\rank (_, _, bet) -> bet * rank) [1 ..]
    >.> sum

scoreHandB :: [CardLabelB] -> Strength
scoreHandB cards =
  scoreHand $ map (\card -> if card == BJ then mostCommonNonJack else card) cards
  where
    cardCounts = sortOn (Data.Ord.Down . snd) (counts cards)
    mostCommonNonJack = maybe BJ fst (find ((/= BJ) . fst) cardCounts)

-- solve the problem (part B)
solveB :: Input -> Int
solveB =
  map (first (map toCardB))
    >.> map (\(hand, bet) -> (scoreHandB hand, hand, bet))
    >.> sortOn (\(strength, hand, _) -> (strength, hand))
    >.> zipWith (\rank (_, _, bet) -> bet * rank) [1 ..]
    >.> sum

test :: String -> Strength
test = map (toCardB . parseCard) >.> scoreHandB