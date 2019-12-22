module Day5 where

import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, newline)
import Text.ParserCombinators.Parsec.Number (int)
import Text.Parsec.Combinator
import Text.Parsec (parse)

import Control.Monad.State.Lazy

import Control.Applicative
import Control.Monad
import Data.List

import Solution

-- Part 1

part1 :: Solution Program Int
part1 = Solution {
    name        = "Day 5 Part 1",
    parser      = inputParser,
    solver      = part1Solver,
    examples    = [],
    inputFile   = "./data/days/Day5.in"
}

part1Solver :: Program -> Int
part1Solver = undefined

-- Part 2

-- Common

programResult :: Program -> Int -> [Int]
programResult p i = (flip evalState) (RunningProgram p i 0 []) $ do
    executeProgram
    getOutput

executeProgram :: State RunningProgram ()
executeProgram = stepProgram >>= (\d -> if d then return () else executeProgram)

stepProgram :: State RunningProgram Bool
stepProgram = do
    (Program xs) <- getProgram
    pc <- getPC
    case xs !! pc of
        99 -> return True
        1 -> (performNumericOp (+)) >> (return False)
        2 -> (performNumericOp (*)) >> (return False)
        3 -> performInputOp >> (return False)
        4 -> performOutputOp >> (return False)

performNumericOp :: (Int -> Int -> Int) -> State RunningProgram ()
performNumericOp op = do
    pc <- getPC
    lVal <- getOperand 0
    rVal <- getOperand 1
    resPos <- getInstruction $ pc + 3
    updateProgram (lVal `op` rVal) resPos
    stepPC 4

performInputOp :: State RunningProgram ()
performInputOp = do
    pc <- getPC
    i <- getInput
    pos <- getInstruction $ pc + 1
    updateProgram i pos
    stepPC 1

performOutputOp :: State RunningProgram ()
performOutputOp = do
    pc <- getPC
    val <- getInstruction $ pc + 1
    addOutput val
    stepPC 1

-- State management

getProgram :: State RunningProgram Program
getProgram = do
    (RunningProgram p _ _ _) <- get
    return p

setProgram :: Program -> State RunningProgram ()
setProgram p = do
    (RunningProgram _ i pc o) <- get
    put $ RunningProgram p i pc o

getInstruction :: Int -> State RunningProgram Int
getInstruction idx = do
    (Program xs) <- getProgram
    return $ xs !! idx

updateProgram :: Int -> Int -> State RunningProgram ()
updateProgram s idx = do
    (Program xs) <- getProgram
    let xs' = replaceNth s idx xs
    setProgram (Program xs')

getInput :: State RunningProgram Int
getInput = do
    (RunningProgram _ i _ _) <- get
    return i

getPC :: State RunningProgram Int
getPC = do
    (RunningProgram _ _ pc _) <- get
    return pc

stepPC :: Int -> State RunningProgram ()
stepPC x = do
    (RunningProgram p i pc o) <- get
    put $ RunningProgram p i (pc + x) o
      

getOutput :: State RunningProgram [Int]
getOutput = do
    (RunningProgram _ _ _ o) <- get
    return o

addOutput :: Int -> State RunningProgram ()
addOutput newOutput = do
    (RunningProgram p i pc o) <- get
    put $ RunningProgram p i pc (o ++ [newOutput])

getOperand :: Int -> State RunningProgram Int
getOperand paramNumber = do
    pc <- getPC
    opCode <- getInstruction pc
    rawOperand <- getInstruction $ pc + paramNumber + 1
    -- 99 is the only opcode which is two digits and it has no parameters
    if getParamMode opCode paramNumber == 0 then 
        -- Position mode
        getInstruction rawOperand
    else
        -- Immediate mode
        return rawOperand
    where 
        getParamMode o n = o `mod` (10^(n + 1)) - (o `mod` (10^n))

-- Datatype

data RunningProgram = RunningProgram {
    program :: Program,
    input :: Int,
    programCounter :: Int,
    output :: [Int]
}
    deriving (Show, Eq)

data Program = Program [Int]
    deriving (Show, Eq)

inputParser :: Parser Program
inputParser = Program <$> (int `sepBy` (char ','))

replaceNth :: a -> Int -> [a] -> [a]
replaceNth s idx xs = lxs ++ s : rxs
    where (lxs, _:rxs) = splitAt idx xs

