-- use like this:
-- pbpaste | runhaskell day18 | ghci day18a
-- pbpaste | runhaskell day18 | ghci day18b
-- yeah its super jank but no way i'm writing a parser lmao
main :: IO ()
main = interact
  $ (\t -> "Prelude.sum [" ++ t ++ "]") . tail . concatMap (',':) . lines