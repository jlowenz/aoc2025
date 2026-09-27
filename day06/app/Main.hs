module Main (main) where

import Lib (
    Part(Part1,Part2),
    part1Parse,
    part2Parse, 
    solveProblems, 
    )

part1 :: String -> String
part1 = show . solveProblems . part1Parse . lines

part2 :: String -> String
part2 = show . solveProblems . part2Parse . lines

main :: IO ()
main = do 
    content <- readFile "input.txt"
    -- putStrLn $ show $ parseProblems (lines content)
    putStrLn $ part1 content
    putStrLn $ part2 content
