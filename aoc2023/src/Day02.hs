module Day02 (solveA, solveB) where

import           Data.List.Split (splitOn)

data Game = Game { getGameId    :: Int
                 , getGamePlays :: [Play]
                 } deriving (Show)

data GameSummary = GameSummary { getSummaryId   :: Int
                               , getSummaryPlay :: Play
                               } deriving (Show)

data Play = Play { r :: Int
                 , g :: Int
                 , b :: Int} deriving (Show)

initPlay :: Play
initPlay = Play 0 0 0

parseGame :: String -> Game
parseGame str =
  let idAndGame = drop 5 str
      [id, gameStr] = splitOn ": " idAndGame
      playStrs = splitOn "; " gameStr
  in  Game (read id) (map parsePlay playStrs)

parsePlay :: String -> Play
parsePlay = foldr incrPlay initPlay . splitOn ", "
  where
    incrPlay playStr acc =
      let [countStr, color] = splitOn " " playStr
          count = read countStr
      in  case color of
            "red"   -> acc { r = count }
            "blue"  -> acc { b = count}
            "green" -> acc { g = count }

maxCountPlay :: [Play] -> Play
maxCountPlay  = foldr1 (\(Play r1 g1 b1) (Play r2 g2 b2) -> Play (max r1 r2) (max g1 g2) (max b1 b2))

maxCountGame ::  Game -> GameSummary
maxCountGame  (Game i plays) = GameSummary i (maxCountPlay plays)

isAcceptable :: GameSummary -> Bool
isAcceptable (GameSummary _ (Play r' g' b')) = r' <= 12 && g' <= 13 && b' <= 14

solveA :: String -> Int
solveA = sum . map getSummaryId . filter isAcceptable . map (maxCountGame . parseGame) . lines

power :: GameSummary -> Int
power (GameSummary _ (Play r' g' b')) = r' * g' * b'

solveB :: String -> Int
solveB = sum . map (power . maxCountGame . parseGame) . lines


