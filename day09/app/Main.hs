module Main (main) where

import Lib

main :: IO ()
main = do 
    r <- part2 "input.txt"
    putStrLn $ show $ r
