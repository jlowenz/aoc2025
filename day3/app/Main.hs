module Main (main) where

import Data.Char (digitToInt)
import Lib

stringToInts :: String -> [Int]
stringToInts = map digitToInt

main :: IO ()
main = do
    contents <- readFile "input.txt"
    -- Part 1
    putStrLn $ show $ sum $ map maxJoltage $ map stringToInts $ lines contents
    -- Part 2
    putStrLn $ show $ sum $ map maxJoltage12 $ map stringToInts $ lines contents

