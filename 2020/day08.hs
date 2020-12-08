import           Data.List.Split (splitOn)
import           Data.Maybe (fromJust)

data Op = Nop
        | Acc
        | Jmp
  deriving (Show, Eq)

type Instruction = (Op, Int)

data State = State { pointer :: Int
                   , accumulator :: Int
                   , instructions :: [(Instruction, Bool)]
                   }
  deriving (Show)

parse :: String -> State
parse = State 0 0 . map (\x -> (toInstruction x, False)) . splitOn "\n"

toInstruction :: String -> Instruction
toInstruction s
  | op == "nop" = (Nop, n)
  | op == "acc" = (Acc, n)
  | op == "jmp" = (Jmp, n)
  where
    [op, n'] = splitOn " " s

    n = read $ filter (/= '+') n'

{-|
  Runs the program until it is going to execute an instruction which
  has already been run, at which point the current accumulator value
  (prior to running the instruction again) is returned.
-}
run :: State -> Int
run (State pnt acc is) =
  let ((op, arg), secondTime) = is !! pnt
  in if secondTime
     then acc
     else case op of
       Nop -> run $ State (pnt + 1) acc (setDone is pnt)
       Acc -> run $ State (pnt + 1) (acc + arg) (setDone is pnt)
       Jmp -> run $ State (pnt + arg) acc (setDone is pnt)

{-|
  Tries running the program to find a halting solution, with one
  instruction swap (Nop <-> Jmp) allowed if the first argument is True.

  Returns Just n where n is the value of the accumulator upon program halt,
  or Nothing if the program loops indefinitely.
-}
runTryChange :: Bool -> State -> Maybe Int
runTryChange canChange (State pnt acc is) =
  if pnt == length is
  then Just acc
  else let ((op, arg), secondTime) = is !! pnt
       in if secondTime
          then Nothing
          else if op == Acc
               then runTryChange canChange
                 $ State (pnt + 1) (acc + arg) (setDone is pnt)
               else firstJust
                 (if canChange
                  then runTryChange False $ State pnt acc (flipOp is pnt)
                  else Nothing)
                 $ case op of
                   Nop -> runTryChange canChange
                     $ State (pnt + 1) acc (setDone is pnt)
                   Jmp -> runTryChange canChange
                     $ State (pnt + arg) acc (setDone is pnt)

firstJust :: Maybe Int -> Maybe Int -> Maybe Int
firstJust (Just a) _ = Just a
firstJust _ b = b

{-|
  Given a list of (instruction, has run?) pairs and an index n,
  changes the nth instruction's has run flag to True.
-}
setDone :: [(Instruction, Bool)] -> Int -> [(Instruction, Bool)]
setDone ((i, _):is) 0 = (i, True):is
setDone (i:is) n = i:setDone is (n - 1)

{-|
  Given a list of (instruction, has run?) pairs and an index n,
  swaps the nth instruction between Nop and Jmp
-}
flipOp :: [(Instruction, Bool)] -> Int -> [(Instruction, Bool)]
flipOp (((Nop, n), f):is) 0 = ((Jmp, n), f):is
flipOp (((Jmp, n), f):is) 0 = ((Nop, n), f):is
flipOp (i:is) n = i:flipOp is (n - 1)

main :: IO ()
main = do
  input <- getContents
  let parsed = parse input
  putStrLn $ "Part A: " ++ show (run parsed)
  putStrLn $ "Part B: " ++ show (fromJust $ runTryChange True parsed)