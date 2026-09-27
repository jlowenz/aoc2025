{-# LANGUAGE QuasiQuotes #-}
module Lib
    ( makeMatrix, findAccessibleRolls, countRolls, clearAllAccessibleRolls
    ) where

import Data.Array.Repa (Z, U, DIM2, (:.)(..), (!), computeP, (*^), (-^))
import Data.Array.Repa.Index (ix2)
import Data.Array.Repa.Stencil (Boundary(..), Stencil(..))
import Data.Array.Repa.Stencil.Dim2 (stencil2, makeStencil2, mapStencil2)
import qualified Data.Array.Repa as R

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Matrix = R.Array U DIM2 Int

makeMatrix :: [[Int]] -> Matrix
makeMatrix els = R.fromListUnboxed sh (concat els)
    where
        cols = length $ head els
        rows = length els
        sh = ix2 rows cols
        
boundary = BoundConst 0

neighbors :: Stencil DIM2 Int
neighbors = [stencil2|1 1 1
                      1 1 1
                      1 1 1|]

doConvolve :: Matrix -> IO Matrix
doConvolve = computeP . mapStencil2 boundary neighbors

findAccessibleRolls :: Matrix -> IO Matrix
findAccessibleRolls m = computeP 
    $ (R.map (\x -> if x==0 then 0 else (if x < 5 then 1 else 0)))
    $ (*^ m) 
    $ mapStencil2 boundary neighbors m

countRolls :: Matrix -> IO Int
countRolls = R.foldAllP (+) 0 

removeAccessibleRolls :: Matrix -> Matrix -> IO Matrix
removeAccessibleRolls accessible all = computeP $ all -^ accessible

clearAllAccessibleRolls :: Int -> Matrix -> IO Int
clearAllAccessibleRolls cleared m = do
    putStrLn $ "Cleared " ++ show cleared 
    accessible <- findAccessibleRolls m
    count <- countRolls accessible
    if count > 0 
        then do
            newLayout <- removeAccessibleRolls accessible m
            clearAllAccessibleRolls (cleared + count) newLayout
        else return cleared


