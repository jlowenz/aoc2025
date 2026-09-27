{-# LANGUAGE OverloadedRecordDot #-}

module Lib where

import Data.List as L
import Data.Map as M
import Data.Set as S

-- just do a big matrix and run the simulation there?
-- or convert the data into a useful structure?

data Splitter = S Int Bool
    deriving Show
newSplitter :: Int -> Splitter
newSplitter n = S n False

type SplitterMap = M.Map Int Splitter
newSplitterMap :: SplitterMap
newSplitterMap = M.empty

data Dir = N | L | R deriving (Show, Ord, Eq)

data Beam = B { worlds :: Integer, n :: Int }
    deriving (Show, Eq)

instance Ord Beam where
    compare b1 b2 = compare b1.n b2.n

newBeam :: Int -> Beam
newBeam n = B 1 n

extendBeam :: Beam -> Dir -> Int -> Beam
extendBeam b _ n = B b.worlds n

beamVal :: Beam -> Int
beamVal b = b.n

type BeamSet = M.Map Int Beam

newBeamSet :: BeamSet
newBeamSet = M.empty

data Level = Lvl {
    splitters :: SplitterMap,
    beams :: BeamSet
} deriving (Show)

emptyLevel :: Level
emptyLevel = Lvl {splitters = newSplitterMap, beams = newBeamSet}

addBeam :: Beam -> Level -> Level
addBeam b l = l { beams = M.alter f b.n l.beams }
    where 
        -- add b to the map
        f Nothing = Just b 
        -- there's something already there... 
        f (Just oldB) = Just (oldB { worlds = oldB.worlds + b.worlds })

addBeams :: [Beam] -> Level -> Level
addBeams [] l = l
addBeams (x:xs) l = addBeams xs $ addBeam x l

addSplitter :: Int -> Level -> Level
addSplitter idx l = l {splitters = M.insert idx (newSplitter idx) l.splitters}

activateSplitter :: Int -> Level -> Level
activateSplitter idx l = l {splitters = M.insert idx (S idx True) l.splitters}

levelFromString :: String -> Level
levelFromString = go 0 emptyLevel
    where
        go :: Int -> Level -> String -> Level
        go _ lvl [] = lvl
        go idx lvl (c:cs) = case c of
            '.' -> go (idx+1) lvl cs
            'S' -> go (idx+1) (addBeam (newBeam idx) lvl) cs
            '^' -> go (idx+1) (addSplitter idx lvl) cs
            _ -> go (idx+1) lvl cs

data Manifold = M {
    levels :: [Level]
} deriving (Show)

emptyManifold :: Manifold
emptyManifold = M { levels = [] }

buildManifold :: [String] -> Manifold
buildManifold ls = M { levels = go ls }
    where
        go :: [String] -> [Level]
        go [] = []
        go (x:xs) = levelFromString x : go xs

-- Idea: each beam in prev propagates to curr
-- lookup beam in curr splitters
--   nothing -> add beam
--   splitter -> update splitter, add beam to left & right
step :: Maybe Level -> Level -> Level
step Nothing lvl = lvl
step (Just prev) curr = M.foldr propagate curr prev.beams
    where
        propagate :: Beam -> Level -> Level
        propagate b l = let n = beamVal b in 
            case M.lookup n l.splitters of
                Nothing -> addBeam (extendBeam b N n) l
                Just (S _ _) -> activateSplitter n (addBeams [left, right] l)
                    where 
                        left = extendBeam b L (n-1)
                        right = extendBeam b R (n+1)

simulate :: Manifold -> Manifold 
simulate (M { levels = lvls }) = M { levels = go Nothing lvls }
    where
        go :: Maybe Level -> [Level] -> [Level]
        go (Just curr) [] = [curr]
        go Nothing (curr:rest) = step Nothing curr : go (Just curr) rest 
        go prev (curr:rest) = steppedCurr : go (Just steppedCurr) rest
            where 
                steppedCurr = step prev curr

countSplits :: Manifold -> Int
countSplits m = L.foldr countActive 0 m.levels
    where
        countActive (Lvl {splitters = s}) acc = 
            M.foldr (\(S _ active) acc -> if active then acc+1 else acc) acc s

computeWorlds :: Manifold -> Integer
computeWorlds m = M.foldr agg 0 (beams $ last m.levels)
    where
        agg :: Beam -> Integer -> Integer
        agg b acc = acc + b.worlds

part1 :: IO ()
part1 = do
    contents <- readFile "day7.txt"
    putStrLn $ show $ countSplits $ simulate $ buildManifold $ lines contents

part2 :: IO ()
part2 = do
    contents <- readFile "day7.txt"
    putStrLn $ show $ computeWorlds $ simulate $ buildManifold $ lines contents
