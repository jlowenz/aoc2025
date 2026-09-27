module Main (main) where

import Lib

main :: IO ()
main = do 
    contents <- readFile "input.txt"
    -- part 1
    putStrLn $ show $ sumInvalid findInvalid $ toRanges contents
    -- part 2
    putStrLn $ show $ sumInvalid findInvalid2 $ toRanges contents
