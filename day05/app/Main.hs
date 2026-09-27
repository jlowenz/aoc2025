module Main (main) where

import Lib

loadDB :: String -> IO IngredientDB
loadDB fname = do 
    contents <- readFile fname
    return $ parseIngredientDB contents

main :: IO ()
main = do 
    idb <- loadDB "input.txt"
    putStrLn $ show $ countFresh idb
    putStrLn $ show $ countPossibleFresh idb

