module Main (main) where

import           Data.List.Extra (trim)
import           Impl

main :: IO ()
main = do
  input <- getContents
  let input' = trim input
  print $ solveA input'
  print $ solveB input'
