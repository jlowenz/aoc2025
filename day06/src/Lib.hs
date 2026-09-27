module Lib where

import Data.Char (isDigit, isSpace)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

trim :: String -> String
trim s = takeWhile (not . isSpace) $ dropWhile isSpace s

data Op = Unknown | Plus | Mult
    deriving (Show, Eq)

parseOp :: String -> Op
parseOp "*" = Mult
parseOp "+" = Plus
parseOp _ = Unknown

data Problem = P [String] Op Int
    deriving (Show)
type Problems = [Problem]

emptyProblem = P [] Unknown

readInteger :: String -> Integer
readInteger = read

solve :: Problem -> Integer
solve (P nums op _) = case op of
        Mult -> product parsedNums
        Plus -> sum parsedNums
        Unknown -> 0
    where parsedNums = map readInteger nums

elementWiseAppend :: [String] -> String -> [String]
elementWiseAppend [] [] = []
elementWiseAppend (x:xs) (c:cs) = (x ++ [c]):(elementWiseAppend xs cs)
elementWiseAppend _ _ = undefined -- these should have the same length

updateProblem :: Problem -> String -> Problem
updateProblem (P nums op w) numStr = P (elementWiseAppend nums (take w numStr)) op w

updateProblems :: Problems -> String -> Problems
updateProblems [] _ = []
updateProblems (p@(P _ _ w):ps) s = (updateProblem p nums):(updateProblems ps rest)
    where
        (nums, rest) = splitAt (w+1) s 

emptyNums :: Int -> [String]
emptyNums n = replicate n ""

makeProblem :: String -> Problem
makeProblem opStr = P nums op w
    where
        w = length opStr - 1
        nums = emptyNums w 
        op = parseOp (trim opStr)

initProblems :: String -> Problems
initProblems s = go (tail s) [(head s)]
    where
        go :: String -> String -> Problems
        go [] opStr = [makeProblem (opStr ++ " ")]
        go (x:xs) opStr 
            | isSpace x = go xs (opStr ++ [x])
            | otherwise = (makeProblem opStr):(go xs [x])

parseProblems :: [String] -> Problems
parseProblems ls = go nums probs
    where
        n = length ls
        (nums, ops) = splitAt (n-1) ls
        probs = initProblems (head ops)
        go :: [String] -> Problems -> Problems
        go [] probs = probs
        go (x:xs) probs = go xs (updateProblems probs x)

solveProblems :: Problems -> Integer
solveProblems = foldr accSolve 0 
    where
        accSolve p acc = (solve p) + acc
