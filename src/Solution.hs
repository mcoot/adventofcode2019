{-# LANGUAGE DuplicateRecordFields #-}

module Solution (Solution(Solution, name, parser, solver, examples, inputFile), test, solve) where

import System.CPUTime
import Control.Exception
import Control.Monad

import Text.Parsec (parse)
import Text.Parsec.String (Parser)

time :: t -> IO (t, Double)
time a = do
    start <- getCPUTime
    res <- evaluate a
    end <- getCPUTime
    let ms = (fromIntegral $ end - start) / (10^9)
    return (res, ms)

idxForM_ :: (Monad m) => [a] -> ((a, Int) -> m b) -> m ()
idxForM_ xs = forM_ (zipWith (,) xs [1..(length xs)])

data Solution a b = Solution {
    name        :: String,
    parser      :: Parser a,
    solver      :: a -> b,
    examples    :: [(String, b)],
    inputFile   :: String
}

test :: (Show a, Show b) => Solution a b -> IO ()
test (Solution name parser solver examples _) = do
    putStrLn $ "Testing problem \"" ++ name ++ "\""
    idxForM_ examples $ \((input, expected), idx) -> do
        case parse parser "" input of
            Left err -> putStrLn $ "Failed to parse: " ++ (show err)
            Right d -> do
                putStrLn $ "Example " ++ (show idx)
                putStrLn $ "\tExpected: " ++ (show expected)
                (result, evalTime) <- time $ solver d
                putStrLn $ "\tActual:   " ++ (show result) ++ " (" ++ (show evalTime) ++ "ms)"

solve :: (Show a, Show b) => Solution a b -> IO ()
solve (Solution name parser solver _ inputFile) = do
    putStrLn $ "Solving problem \"" ++ name ++ "\""
    input <- readFile inputFile
    case parse parser "" input of
        Left err -> putStrLn $ "Failed to parse: " ++ (show err)
        Right d -> do
            (result, evalTime) <- time $ solver d
            putStrLn $ "Result: " ++ (show result) ++ " (" ++ (show evalTime) ++ "ms)"