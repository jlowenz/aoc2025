module Main (main) where

import Lib

main :: IO ()
main = do 
    -- part 1
    r1 <- part1 "input.txt"
    putStrLn $ show $ rArea r1
    -- part 2
    r2 <- part2 "input.txt"
    putStrLn $ show $ rArea r2
