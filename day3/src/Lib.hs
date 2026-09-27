module Lib
    ( maxJoltage, insertDigitAndDrop, maxJoltage12
    ) where

data JoltState = J Int Int Integer

-- Part 1
-- Idea: there is some split where the max num on left and max num on right is the 
-- largest number. We need to store the left max, right max, and max so far as we
-- walk through the list... 
maxJoltage :: [Int] -> Integer
maxJoltage [] = 0
maxJoltage (x:xs) = go 1 rst (J l r (toInteger (l*10 + r)))
    where 
        rst = xs
        n = 1 + length xs
        l = x
        r = maximum rst
        go :: Int -> [Int] -> JoltState -> Integer
        go _ [] (J _ _ jolt) = jolt
        go split (y:ys) (J ll rr jolt) 
            | split == (n-1) = jolt
            | split < (n-1) = go (split+1) rest (J newL newR (max jolt newJolt))
            | otherwise = jolt
                where 
                    rest = ys
                    newL = max ll y
                    newR = if newL < rr then rr else maximum rest
                    newJolt = toInteger (newL*10 + newR)


maxBatteries :: Int
maxBatteries = 12
type Digits = [Int]
-- store the current set of batteries, 
-- the remaining digits
data JoltSt = Js Digits Int 

-- Inserts a digit into digits when it's greater than an element, 
-- and drops the following digits
insertDigitAndDrop :: Int -> Int -> Digits -> Digits
insertDigitAndDrop _ digit [] = [digit]
insertDigitAndDrop maxBatt digit (d:ds) 
    | digit > d = [digit]
    | otherwise = take maxBatt (d:(insertDigitAndDrop maxBatt digit ds))

-- Part 2
maxJoltage12 :: [Int] -> Integer
maxJoltage12 allDigits = go allDigits (Js [] (length allDigits))
    where
        go :: Digits -> JoltSt -> Integer
        go [] (Js digits _) = foldl (\x y -> x*10 + y) 0 (map toInteger digits)
        go (d:ds) (Js digits n) 
            | n >= maxBatteries = go ds (Js (insertDigitAndDrop maxBatteries d digits) (n-1))
            | otherwise = go ds (Js (hd ++ (insertDigitAndDrop n d tl)) (n-1))
                where (hd, tl) = splitAt (maxBatteries - n) digits