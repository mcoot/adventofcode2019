module Day4 where

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

part1 :: Solution (Int, Int) Int
part1 = Solution {
    name        = "Day 4 Part 1",
    parser      = inputParser,
    solver      = part1Solver,
    examples    = [
        ("111111-111111", 1),
        ("223450-223450", 0),
        ("123789-123789", 0)
    ],
    inputFile   = "./data/days/Day4.in"
}

part1Solver :: (Int, Int) -> Int
part1Solver (low, high) = numValid isValid1 low high

isValid1 :: Int -> Bool
isValid1 x = nonDecreasing x &&  hasDoubleDigits x

hasDoubleDigits :: Int -> Bool
hasDoubleDigits x = any (uncurry (==)) ((digits x) `zip` (drop 1 $ digits x))

-- Part 2

part2 :: Solution (Int, Int) Int
part2 = Solution {
    name        = "Day 4 Part 2",
    parser      = inputParser,
    solver      = part2Solver,
    examples    = [
        ("112233-112233", 1),
        ("123444-123444", 0),
        ("111122-111122", 1)
    ],
    inputFile   = "./data/days/Day4.in"
}

part2Solver :: (Int, Int) -> Int
part2Solver (low, high) = numValid isValid2 low high

isValid2 :: Int -> Bool
isValid2 x = nonDecreasing x && hasValidDoubleDigits x

hasValidDoubleDigits :: Int -> Bool
hasValidDoubleDigits x = any id $ f <$> movingWindow
    where 
        movingWindow = (zip4 pds (drop 1 pds) (drop 2 pds) (drop 3 pds))
        pds = -1 : (digits x) ++ [-1]
        f (a, b, c, d) = b == c && a /= b && c /= d

-- Common

numValid :: (Int -> Bool) -> Int -> Int -> Int
numValid validator low high = sum $ const 1 <$> (findAllValid validator low high)

findAllValid :: (Int -> Bool) -> Int -> Int -> [Int]
findAllValid validator low high = unfoldr getNextMaybe (low - 1)
    where getNextMaybe c
            | n > high  = Nothing
            | otherwise = Just (n, n)
            where n = nextValid validator c

nextValid :: (Int -> Bool) -> Int -> Int
nextValid validator cur
    | validator next = next -- Found one matching both criteria
    | otherwise      = nextValid validator next -- Found one that is non-decreasing but lacks double digits
    where
        doInc ds p = l ++ (take (length r + 1) $ repeat $ x + 1)
            where
                (l, x:r) = splitAt p ds
        ds = digits cur
        placeToIncrement = ((-) (length ds - 1)) <$> (findIndex ((/=) 9) (reverse ds))
        nextDigits = case placeToIncrement of
            Nothing -> take (length ds + 1) $ repeat 1 -- Increase digit count (in practice our ranges are always 6 digits)
            Just p  -> doInc ds p
        next = digitsToNum nextDigits


nextValidNaive :: (Int -> Bool) -> Int -> Int
nextValidNaive validator cur = (flip (!!) 0) $ dropWhile (not . validator) $ [cur+1..]

digits :: Int -> [Int]
digits x = ((flip mod 10) . (div x)) <$> (reverse $ takeWhile (< x) $ [10^i|i <-[0..]])

digitsToNum :: [Int] -> Int
digitsToNum xs = sum $ fmap getPlace indexed 
    where 
        indexed = (zipWith (,) (reverse xs) [0..(length xs - 1)])
        getPlace (x, idx) = x * (10 ^ idx)

nonDecreasing :: Int -> Bool
nonDecreasing x = all (uncurry (<=)) ((digits x) `zip` (drop 1 $ digits x))

inputParser :: Parser (Int, Int)
inputParser = (,) <$> int <*> (char '-' *> int)