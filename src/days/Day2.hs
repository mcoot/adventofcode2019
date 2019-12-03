module Day2 where

import Text.Parsec.String (Parser)
import Text.Parsec.Char (char)
import Text.ParserCombinators.Parsec.Number (int)
import Text.Parsec.Combinator

import Control.Monad.State.Lazy

import Solution

-- Part 1

part1 :: Solution Program Int
part1 = Solution {
    name        = "Day 1 Part 1",
    parser      = inputParser,
    solver      = part1Solver,
    examples    = [],
    inputFile   = "./data/days/Day2.in"
}

part1Solver :: Program -> Int
part1Solver = programResult . part1Substitute

part1Substitute :: Program -> Program
part1Substitute (Program xs) = Program $ (replaceNth 12 1) $ replaceNth 2 2 xs

programResult :: Program -> Int
programResult p = (flip evalState) (RunningProgram p 0) $ do
    executeProgram
    RunningProgram (Program xs) _ <- get
    return $ xs !! 0

executeProgram :: State RunningProgram ()
executeProgram = do
    done <- stepProgram
    if done then
        return ()
    else
        executeProgram

stepProgram :: State RunningProgram Bool
stepProgram = do
    RunningProgram (Program xs) pc <- get
    case xs !! pc of
        99 -> return True
        1 -> (performNumericOp (+)) >> (return False)
        2 -> (performNumericOp (*)) >> (return False)

performNumericOp :: (Int -> Int -> Int) -> State RunningProgram ()
performNumericOp op = do
    RunningProgram (Program xs) pc <- get
    let lPos = xs !! (pc + 1)
    let rPos = xs !! (pc + 2)
    let resPos = xs !! (pc + 3)
    let result = (xs !! lPos) `op` (xs !! rPos)
    let xs' = replaceNth result resPos xs
    put $ RunningProgram (Program xs') (pc + 4)
    return ()

-- Part 2

part2Solver :: Program -> Int
part2Solver = undefined

-- Common

data RunningProgram = RunningProgram {
    program :: Program,
    programCounter :: Int
}
    deriving (Show, Eq)

newtype Program = Program [Int]
    deriving (Show, Eq)

replaceNth :: a -> Int -> [a] -> [a]
replaceNth s idx xs = lxs ++ s : rxs
    where (lxs, _:rxs) = splitAt idx xs

inputParser :: Parser Program
inputParser = Program <$> (int `sepBy` (char ','))