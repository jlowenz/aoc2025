module Lib (
    Part(Part1,Part2),
    solveProblems,
    part1Parse, 
    part2Parse
) where

import Data.Char (isSpace)

trim :: String -> String
trim s = takeWhile (not . isSpace) $ dropWhile isSpace s


data Part = Part1 | Part2

data Op = Unknown | Plus | Mult
    deriving (Show, Eq)

parseOp :: String -> Op
parseOp "*" = Mult
parseOp "+" = Plus
parseOp _ = Unknown

data Problem = P [String] Op Int
    deriving (Show)
type Problems = [Problem]

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

updateProblem1 :: Problem -> String -> Problem
updateProblem1 (P nums op w) numStr = P (numStr:nums) op w

updateProblems1 :: Problems -> String -> Problems
updateProblems1 probs nums = go probs (words nums)
    where
        go :: Problems -> [String] -> Problems
        go ps [] = ps
        go (p:ps) (num:rest) = (updateProblem1 p num):(go ps rest)
        go [] _ = []
    
emptyNums :: Int -> [String]
emptyNums n = replicate n ""

makeProblem :: Part -> String -> Problem
makeProblem part opStr = P nums op w
    where
        w = length opStr - 1
        nums = case part of 
            Part1 -> []
            Part2 -> emptyNums w 
        op = parseOp (trim opStr)

initProblems :: Part -> String -> Problems
initProblems _ [] = undefined -- should not be called with empty string
initProblems part (s:ss) = go ss [s]
    where
        go :: String -> String -> Problems
        go [] opStr = [makeProblem part (opStr ++ " ")]
        go (x:xs) opStr 
            | isSpace x = go xs (opStr ++ [x])
            | otherwise = (makeProblem part opStr):(go xs [x])

parseProblems :: Part -> (Problems -> String -> Problems) -> [String] -> Problems
parseProblems part updateFn ls = result
    where
        n = length ls
        result = case splitAt (n-1) ls of
            (nums, (op:_)) -> go nums (initProblems part op)
            _ -> []
        go :: [String] -> Problems -> Problems
        go [] probs = probs
        go (x:xs) probs = go xs (updateFn probs x)

part1Parse :: [String] -> Problems
part1Parse = parseProblems Part1 updateProblems1
part2Parse :: [String] -> Problems
part2Parse = parseProblems Part2 updateProblems

solveProblems :: Problems -> Integer
solveProblems = foldr accSolve 0 
    where
        accSolve p acc = (solve p) + acc
