{-# LANGUAGE OverloadedRecordDot #-}
module Lib where

import Data.Char (isDigit)
import Data.List.Split (splitOn)

type LowerBound = Integer
type UpperBound = Integer
data Interval = Ival LowerBound UpperBound
    deriving (Eq)

instance Show Interval where
    show (Ival lo hi) = "[" ++ show lo ++ "," ++ show hi ++ "]"

instance Ord Interval where
    compare (Ival l1 _) (Ival l2 _) = compare l1 l2

containment :: Integer -> Interval -> Ordering
containment i (Ival lo hi)
    | i < lo = LT
    | i > hi = GT
    | otherwise = EQ

ivalSize :: Interval -> Integer
ivalSize (Ival lo hi) 
    | lo <= hi = hi - lo + 1
    | otherwise = 0

data IntervalRelation = Equal
    | Before
    | After
    | Meets
    | MetBy
    | Overlaps
    | OverlappedBy
    | Starts
    | StartedBy
    | During
    | Contains
    | Finishes
    | FinishedBy
    deriving (Show, Eq, Ord)

relationOf :: Interval -> Interval -> IntervalRelation
relationOf (Ival l1 u1) (Ival l2 u2) 
    | l1 == l2 && u1 == u2 = Equal
    | (u1+1) < l2 = Before 
    | (u2+1) < l1 = After
    | (u1+1) == l2 = Meets
    | (u2+1) == l1 = MetBy
    | l1 > l2 && u1 < u2 = During
    | l2 > l1 && u2 < u1 = Contains
    | u1 >= l2 && u1 < u2 && l1 < l2 = Overlaps
    | u2 >= l1 && u2 < u1 && l2 < l1 = OverlappedBy
    | l1 == l2 && u1 < u2 = Starts
    | l1 == l2 && u2 < u1 = StartedBy
    | l1 > l2 && u1 == u2 = Finishes
    | l1 < l2 && u1 == u2 = FinishedBy
    | otherwise = undefined

type Intervals = [Interval]

mergeInterval :: IntervalRelation -> Interval -> Interval -> Interval
mergeInterval r i1@(Ival l1 u1) i2@(Ival l2 u2) = case r of 
    Equal -> i1
    Meets -> Ival l1 u2
    MetBy -> Ival l2 u1
    During -> i2
    Contains -> i1
    Overlaps -> Ival l1 u2
    OverlappedBy -> Ival l2 u1
    Starts -> i2
    StartedBy -> i1
    Finishes -> i2
    FinishedBy -> i1
    otherwise -> undefined

insertInterval :: Interval -> Intervals -> Intervals
insertInterval ins [] = [ins]
insertInterval ins (i:is) = case relationOf ins i of
    Equal -> i:is
    -- before/starting
    Before -> ins:i:is
    Meets -> (mergeInterval Meets ins i):is
    Starts -> (mergeInterval Starts ins i):is
    FinishedBy -> (mergeInterval FinishedBy ins i):is
    During -> (mergeInterval During ins i):is
    Overlaps -> (mergeInterval Overlaps ins i):is
    
    -- after/ending
    After -> i:(insertInterval ins is)
    StartedBy -> insertInterval (mergeInterval StartedBy ins i) is
    MetBy -> insertInterval (mergeInterval MetBy ins i) is
    Finishes -> (mergeInterval Finishes ins i):is
    Contains -> insertInterval (mergeInterval Contains ins i) is
    OverlappedBy -> insertInterval (mergeInterval OverlappedBy ins i) is

-- construct a list of sorted&merged intervals from unsorted/unmerged intervals
buildIntervals :: Intervals -> Intervals
buildIntervals input = foldr insertInterval [] input

data IntervalTree = Leaf | Node Interval IntervalTree IntervalTree
    deriving (Show)

foldIntervalTree f acc Leaf = acc
foldIntervalTree f acc (Node item left right) = 
    f item (foldIntervalTree f (foldIntervalTree f acc left) right)

-- produce a balanced binary tree of intervals
intervalTree :: Intervals -> IntervalTree
intervalTree ivals = go ivals (length ivals)
    where
        go [] _ = Leaf
        go is n 
            | n > 2 = let leftN = div n 2
                          (left, (center:right)) = splitAt leftN is
                          rightN = if even n then (leftN - 1) else leftN in
                Node center (go left leftN) (go right rightN)
            | n == 2 = Node (head is) Leaf (go (tail is) 1)
            | n == 1 = Node (head is) Leaf Leaf

isFresh :: IntervalTree -> Integer -> Bool
isFresh (Leaf) _ = False
isFresh (Node ival left right) ingredient = case containment ingredient ival of
    LT -> isFresh left ingredient
    GT -> isFresh right ingredient
    EQ -> True

data IngredientDB = Idb { 
    freshRanges :: IntervalTree,
    ingredients :: [Integer] }
    deriving (Show)

splitOnFirstEmpty :: [String] -> Maybe ([String],[String])
splitOnFirstEmpty = go []
    where
        go :: [String] -> [String] -> Maybe ([String],[String])
        go ret [] = Nothing
        go l (x:xs) 
            | x == "" = Just (l, xs)
            | isDigit (head x) = go (x:l) xs

readInteger :: String -> Integer
readInteger = read

parseIngredientInterval :: String -> Interval
parseIngredientInterval s = Ival lo hi
    where
        (lo:hi:_) = fmap readInteger (splitOn "-" s)

parseIngredientDB :: String -> IngredientDB
parseIngredientDB contents = Idb {freshRanges = ivals, ingredients = ingreds}
    where
        (rawIntervals, rawIngreds) = case splitOnFirstEmpty $ lines contents of
            Nothing -> error "bad parse"
            Just (a, b) -> (a, b)
        ingreds = map (read::String -> Integer) rawIngreds
        ivals = intervalTree $ buildIntervals $ map parseIngredientInterval rawIntervals

countFresh :: IngredientDB -> Integer
countFresh idb = 
    foldr (\i acc -> if isFresh idb.freshRanges i then 1+acc else acc) 0 idb.ingredients

countPossibleFresh :: IngredientDB -> Integer
countPossibleFresh idb = foldIntervalTree accFresh 0 idb.freshRanges
    where
        accFresh ival acc = acc + (ivalSize ival)
