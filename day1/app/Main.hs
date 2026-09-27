module Main (main) where

import Lib

main :: IO ()
main = do
    rots <- readRotations "input.txt"
    -- part 1 output
    print $ turnDial (Dial 50) rots turn 
    -- part 2 output
    print $ turnDial (Dial 50) rots turn0x434 -- iterative
    print $ turnDial (Dial 50) rots turnFull -- direct 
