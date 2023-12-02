module Main (main) where

import Day01

main :: IO ()
main = do
  input <- getContents
  putStrLn $ show (solveA input)
  putStrLn $ show (solveB input)
