module Day1 where

import Text.Parsec.String (Parser)
import Text.Parsec.Char (newline)
import Text.ParserCombinators.Parsec.Number (int)
import Text.Parsec.Combinator

import Solution

-- Part 1

part1 :: Solution [Int] Int
part1 = Solution {
    name        = "Day 1 Part 1",
    parser      = inputParser,
    solver      = part1Solver,
    examples    = [("12", 2), ("14", 2), ("1969", 654), ("100756", 33583)],
    inputFile   = "./data/days/Day1.in"
}

part1Solver :: [Int] -> Int
part1Solver = sum . fmap calculateFuel

-- Part 2

part2 :: Solution [Int] Int
part2 = Solution {
    name        = "Day 1 Part 2",
    parser      = inputParser,
    solver      = part2Solver,
    examples    = [("14", 2), ("1969", 966), ("100756", 50346)],
    inputFile   = "./data/days/Day1.in"
}

part2Solver :: [Int] -> Int
part2Solver = sum . fmap calculateFuel2

calculateFuel2 :: Int -> Int
calculateFuel2 0 = 0
calculateFuel2 x = f + (calculateFuel2 f)
    where f = max 0 (calculateFuel x)

-- Common

calculateFuel :: Int -> Int
calculateFuel x = (x `div` 3) - 2

inputParser :: Parser [Int]
inputParser = int `sepBy` newline