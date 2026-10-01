module Main (main) where

import Lib

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let g = buildGraph contents
        node = stringToNode
    -- part 1
    putStrLn $ show $ countPaths g 1 (node "you") (node "out") 0
    -- part 2
    putStrLn $ show $ countPaths g 1 (node "svr") (node "fft") 0
                    * countPaths g 1 (node "fft") (node "dac") (node "svr")
                    * countPaths g 1 (node "dac") (node "out") (node "svr")
