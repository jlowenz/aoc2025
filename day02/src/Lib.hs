{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( repeating, findInvalid, findInvalid2, toInterval, toRanges, sumInvalid
    ) where

import GHC.Stack ( HasCallStack ) 
import Data.Maybe (mapMaybe)
import Data.Text (Text, splitOn, pack, unpack)


isInvalid :: String -> Maybe Integer
isInvalid num = if odd n then Nothing else match (n `div` 2) num
    where 
        n = length num
        match :: Int -> String -> Maybe Integer
        match h digits = if (take h digits) == (drop h digits) then Just (read num) else Nothing

repeating :: HasCallStack => String -> Int -> String -> Int -> Bool
repeating _ _ [] _ = True
repeating _ 0 _ _ = False
repeating (x:_) 1 digits n = (replicate n x) == digits
repeating pat sz digits n = 
    if (mod n sz) /= 0 then False else h == pat && repeating pat sz next (n-sz)
    where 
        (h, next) = splitAt sz digits

-- So, the new invalid check needs to be a lot smarter. Factors?
-- 2 -> 1
-- 3 -> 1
-- 4 -> 1,2
-- 5 -> 1
-- 6 -> 1,2,3
-- 7 -> 1
-- 8 -> 1,2,4
-- 9 -> 1,3
isInvalid' :: HasCallStack => String -> Maybe Integer
isInvalid' digits = if any id (map match sizes) then Just (read digits) else Nothing
    where
        n = length digits
        h = div n 2
        sizes = if h >= 2 then [h, h-1..1] else [h]
        match :: Int -> Bool
        match sz = repeating pat sz rest (n-sz)
            where
                (pat, rest) = splitAt sz digits

findInvalidBase :: (String -> Maybe Integer) -> Interval -> [Integer]
findInvalidBase isInvalidFn (Ival start end) = mapMaybe isInvalidFn $ map show [start..end] 

findInvalid :: Interval -> [Integer]
findInvalid = findInvalidBase isInvalid

findInvalid2 :: Interval -> [Integer]
findInvalid2 = findInvalidBase isInvalid'

data Interval = Ival Integer Integer
    deriving (Show, Eq, Ord)

toInterval :: Text -> Interval
toInterval t = case map (read . unpack) $ splitOn "-" t of
            [a, b] -> Ival a b
            _ -> Ival 0 0 

toRanges :: String -> [Interval]
toRanges s = map toInterval $ splitOn "," $ pack s

sumInvalid :: (Interval -> [Integer]) -> [Interval] -> Integer
sumInvalid findInvalidFn = sum . concat . map findInvalidFn 