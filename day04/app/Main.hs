module Main (main) where

import Lib

charToInt :: Char -> Int
charToInt '@' = 1
charToInt _ = 0

readData :: String -> IO [[Int]]
readData fname = do 
    contents <- readFile fname
    return $ map (map charToInt) $ lines contents

main :: IO ()
main = do
    layout <- readData "input.txt"
    let l = makeMatrix layout
    -- part 1
    accessible <- findAccessibleRolls l
    reachable <- countRolls accessible
    putStrLn $ show reachable
    -- part 2
    rolls <- clearAllAccessibleRolls 0 l
    putStrLn $ show rolls
