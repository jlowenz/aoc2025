module Main (main) where

import Lib

part1 :: String -> String
part1 = show . solveProblems . parseProblems . lines

main :: IO ()
main = do 
    content <- readFile "input.txt"
    -- putStrLn $ show $ parseProblems (lines content)
    putStrLn $ part1 content
