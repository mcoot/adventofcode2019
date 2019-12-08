module Day3 where

import Debug.Trace

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

part1 :: Solution WirePaths Int
part1 = Solution {
    name        = "Day 3 Part 1",
    parser      = inputParser,
    solver      = part1Solver,
    examples    = [
        ("R8,U5,L5,D3\nU7,R6,D4,L4", 6),
        ("R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83", 159),
        ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7", 135)
    ],
    inputFile   = "./data/days/Day3.in"
}

part1Solver :: WirePaths -> Int
part1Solver (WirePaths (p1, p2)) = minimum $ (uncurry distFromOrigin) <$> (findPathIntersections (walkPath p1) (walkPath p2))

-- Part 2

part2 :: Solution WirePaths Int
part2 = Solution {
    name        = "Day 3 Part 2",
    parser      = inputParser,
    solver      = part2Solver,
    examples    = [
        ("R8,U5,L5,D3\nU7,R6,D4,L4", 30),
        ("R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83", 610),
        ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7", 410)
    ],
    inputFile   = "./data/days/Day3.in"
}

part2Solver = minimum . allStepsToIntersections

allStepsToIntersections :: WirePaths -> [Int]
allStepsToIntersections (WirePaths (p1, p2)) = zipWith (+) steps1 steps2
    where steps1 = (stepsToPoint w1) <$> intersections
          steps2 = (stepsToPoint w2) <$> intersections
          w1 = walkPath p1
          w2 = walkPath p2
          intersections = findPathIntersections w1 w2

stepsToPoint :: [(Int, Int)] -> (Int, Int) -> Int
stepsToPoint w pt = case res of
                        Left r  -> r
                        Right r -> r -- Failed to find it; should error handle this but yolo
    where res = foldM (f pt) 0 (w `zip` (drop 1 w))
          f pt acc (wa, wb) = case stepsToPointAlongSegment wa wb pt of
                                Just dist -> Left $ acc + dist
                                Nothing   -> Right $ acc + segmentLength wa wb


segmentLength :: (Int, Int) -> (Int, Int) -> Int
segmentLength a b = maybe 0 id (stepsToPointAlongSegment a b b)

stepsToPointAlongSegment :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Maybe Int
stepsToPointAlongSegment (seg_x1, seg_y1) (seg_x2, seg_y2) (x, y)
    | seg_y1 == seg_y2 = if seg_y1 == y && inbounds x seg_xl seg_xh then Just $ abs (x - seg_x1) else Nothing -- horizontal segment
    | seg_x1 == seg_x2 = if seg_x1 == x && inbounds y seg_yl seg_yh then Just $ abs (y - seg_y1) else Nothing -- vertical segment
    | otherwise = Nothing
    where (seg_xl, seg_yl) = (min seg_x1 seg_x2, min seg_y1 seg_y2)
          (seg_xh, seg_yh) = (max seg_x1 seg_x2, max seg_y1 seg_y2)
          inbounds t c d = t >= c && t <= d

-- Common

findPathIntersections :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
findPathIntersections w1 w2 = (flip (\\) [(0, 0)]) $ foldl (\acc (a, b) -> acc ++ (intersectionsInSegment w2 a b)) [] (w1 `zip` (drop 1 w1))
intersectionsInSegment walk w1a w1b = foldl (\acc (w2a, w2b) -> maybe acc (flip (:) acc) (lineIntersect w1a w1b w2a w2b)) [] (walk `zip` (drop 1 walk))

interpolateWalk :: [(Int, Int)] -> [(Int, Int)]
interpolateWalk walk = foldl (\acc (from, to) -> acc ++ (drop 1 $ interpolatePos from to)) [] (walk `zip` (drop 1 walk)) 

-- Find the intersection of two horizontal/vertical lines if there is one
-- Doesn't consider lines of the same direction that overlap...
lineIntersect :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
lineIntersect (a_x1, a_y1) (a_x2, a_y2) (b_x1, b_y1) (b_x2, b_y2)
    | a_o == 0 && b_o == 1 = if inbounds a_yl b_yl b_yh && inbounds b_xl a_xl a_xh then Just (b_xl, a_yl) else Nothing -- a horizontal b vertical
    | a_o == 1 && b_o == 0 = if inbounds b_yl a_yl a_yh && inbounds a_xl b_xl b_xh then Just (a_xl, b_yl) else Nothing -- b horizontal a vertical
    | otherwise = Nothing
    where a_o = orientation a_x1 a_y1 a_x2 a_y2
          (a_xl, a_yl) = (min a_x1 a_x2, min a_y1 a_y2)
          (a_xh, a_yh) = (max a_x1 a_x2, max a_y1 a_y2)
          b_o = orientation b_x1 b_y1 b_x2 b_y2
          (b_xl, b_yl) = (min b_x1 b_x2, min b_y1 b_y2)
          (b_xh, b_yh) = (max b_x1 b_x2, max b_y1 b_y2)
          orientation x1 y1 x2 y2
            | x1 == x2  = 1 -- vertical
            | otherwise = 0 -- horizontal
          inbounds t c d = t >= c && t <= d

walkPath :: Path -> [(Int, Int)]
walkPath (Path moves) = scanl executeMove (0, 0) moves

interpolatePos :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
interpolatePos (x1, y1) (x2, y2)
    | x1 == x2 && y1 < y2  = (repeat x1) `zip` [y1..y2]
    | x1 == x2 && y1 >= y2 = (repeat x1) `zip` [y1,y1-1..y2]
    | y1 == y2 && x1 < x2  = [x1..x2] `zip` (repeat y1)
    | y1 == y2 && x1 >= x2 = [x1,x1-1..x2] `zip` (repeat y1)
    | otherwise            = [(x1, y1), (x2, y2)]

executeMove :: (Int, Int) -> Move -> (Int, Int)
executeMove (x, y) (Move dir dist) = case dir of
    DirLeft -> (x - dist, y)
    DirUp -> (x, y + dist)
    DirRight -> (x + dist, y)
    DirDown -> (x, y - dist)

distFromOrigin :: Int -> Int -> Int
distFromOrigin x y = (abs x) + (abs y)

data Direction = DirLeft | DirUp | DirRight | DirDown
    deriving (Eq, Show)

data Move = Move {
    direction :: Direction,
    distance :: Int
}
    deriving (Eq, Show)

newtype Path = Path [Move]
    deriving (Eq, Show)

newtype WirePaths = WirePaths (Path, Path)
    deriving (Eq, Show)

dirParser :: Parser Direction
dirParser =  (char 'L' >> return DirLeft) 
         <|> (char 'U' >> return DirUp) 
         <|> (char 'R' >> return DirRight) 
         <|> (char 'D' >> return DirDown)

moveParser :: Parser Move
moveParser = do
    dir <- dirParser
    dist <- int
    return $ Move dir dist

pathParser :: Parser Path
pathParser = Path <$> moveParser `sepBy` (char ',')

inputParser :: Parser WirePaths
inputParser = (curry WirePaths) <$> pathParser <*> (newline >> pathParser)
