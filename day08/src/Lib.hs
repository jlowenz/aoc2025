module Lib where

import Control.Monad (foldM, forM_)
import qualified Data.Array as A
import qualified Data.IORef as R
import Data.List.Split (splitOn)
import qualified Data.Map as M 
import qualified Data.Set as S
import qualified Data.PQueue.Prio.Min as Q
import qualified Data.PQueue.Min as Min
import GHC.Base (Double, undefined)

data Point3 = P3 Int Int Int
    deriving (Show, Eq, Ord)

dist :: Point3 -> Point3 -> Double
dist (P3 x1 y1 z1) (P3 x2 y2 z2) = sqrt $ (dt x2 x1) + (dt y2 y1) + (dt z2 z1)
    where 
        dt :: Int -> Int -> Double
        dt a b = (fromIntegral (a-b))^(2::Int)

type Points = A.Array Int Point3

-- Read the points into an array
readPoints :: String -> Points
readPoints content = A.listArray (1,n) $ (Prelude.map toPoint) $ Prelude.map (splitOn ",") ls
    where
        ls = lines content
        n = length ls
        toPoint :: [String] -> Point3
        toPoint [x,y,z] = P3 (read x) (read y) (read z)


type PointPair = (Int,Int)
type DistQueue = Q.MinPQueue Double PointPair

maxPairs :: Int
maxPairs = 1000

-- prune :: DistQueue -> DistQueue
-- prune q = if Q.size q > maxPairs then Q.drop 1 q else q

addPt :: PointPair -> Points -> DistQueue -> DistQueue
addPt pr@(i,j) pts q = Q.insert d pr q
    where 
        d = dist (pts A.! i) (pts A.! j)

-- 1000*999/2 = 499500 distances
-- Compute the 1000 smallest distances (using a priority queue)
-- Part2: store all distances
findMinPairs :: Points -> DistQueue
findMinPairs p = go Q.empty [(x,y) | x <- [1..n], y <- [x+1..n]] p 
    where
        n = snd $ A.bounds p
        go :: DistQueue -> [PointPair] -> Points -> DistQueue
        go q [] _ = q
        go q (x:xs) pts = go (addPt x pts q) xs pts


type IndexSet = S.Set Int
type Component = R.IORef IndexSet 
type PtMap = M.Map Int Component

newComponent :: PointPair -> IO Component
newComponent (i,j) = R.newIORef $ S.insert i (S.singleton j)

compSize :: Component -> IO Int
compSize c = do 
    iset <- R.readIORef c -- maybe this isn't necessary?
    return $ S.size iset

addToComponent :: Int -> Component -> IO Int
addToComponent i c = do
    iset <- R.readIORef c
    let uset = S.insert i iset
    R.writeIORef c uset
    return $ S.size uset

mergeComponents :: Component -> Component -> IO (Component, Int)
mergeComponents a b = do
    aSet <- R.readIORef a
    bSet <- R.readIORef b
    let uset = S.union aSet bSet
    R.writeIORef a uset
    return $ (a, S.size uset)

updateMap :: PtMap -> IndexSet -> Component -> PtMap
updateMap m indices c = S.foldr go m indices
    where
        go :: Int -> PtMap -> PtMap 
        go i pm = M.insert i c pm

insertItems :: [(Int, Component)] -> PtMap -> PtMap
insertItems [] m = m
insertItems ((i,c):xs) m = insertItems xs (M.insert i c m)

type AggResult = (PtMap, PointPair)

-- Aggregate pairs
-- Until we find a full component of size n
aggregate :: Int -> DistQueue -> IO AggResult
aggregate n = foldM go (M.empty, (-1,-1))
    where 
        go :: AggResult -> PointPair -> IO AggResult
        go (m, (-1,-1)) (i,j) = 
            let ic = M.lookup i m
                jc = M.lookup j m 
            in case (ic, jc) of 
                (Nothing, Nothing) -> do
                    c <- newComponent (i,j)
                    return $ (insertItems [(i,c),(j,c)] m, (-1,-1))
                (Just c, Nothing) -> do
                    sz <- addToComponent j c
                    let newm = M.insert j c m
                    return $ if sz == n 
                        then (newm, (i,j))
                        else (newm, (-1,-1))
                (Nothing, Just c) -> do
                    sz <- addToComponent i c
                    let newm = M.insert i c m
                    return $ if sz == n then (newm, (i,j)) else (newm, (-1,-1))
                (Just ci, Just cj) -> do 
                    iSize <- compSize ci
                    jSize <- compSize cj 
                    if iSize > jSize then 
                        do 
                            (newC, sz) <- mergeComponents ci cj
                            jset <- R.readIORef cj
                            let newm = updateMap m jset newC
                            return $ if sz == n then (newm, (i,j)) else (newm, (-1,-1))
                    else 
                        do
                            (newC, sz) <- mergeComponents cj ci
                            iset <- R.readIORef ci
                            let newm = updateMap m iset newC
                            return $ if sz == n then (newm, (i,j)) else (newm, (-1,-1))
        go result _ = return result -- skip all additional processing

-- Support "identified" components
-- Component could have been a more defined item
-- then we wouldn't necessarily need this (refactor?)
data CompId = CompId Int Int Component
instance Show CompId where
    show (CompId a as _) = "CompID " ++ (show a) ++ "," ++ (show as)
instance Eq CompId where
    (==) (CompId a _ _) (CompId b _ _) = a == b
instance Ord CompId where
    compare (CompId a as _) (CompId b bs _) = case compare as bs of
        LT -> LT
        EQ -> compare a b
        GT -> GT

newCompId :: Component -> IO CompId
newCompId c = do
    iset <- R.readIORef c
    return $ CompId (S.findMin iset) (S.size iset) c

type CompQueue = S.Set CompId
numComponents :: Int
numComponents = 3

pruneC q = if S.size q > numComponents then S.deleteMin q else q

findMaxComponents :: PtMap -> IO CompQueue
findMaxComponents = foldM addCompId S.empty 
    where
        addCompId :: CompQueue -> Component -> IO CompQueue
        addCompId q c = do 
            cid <- newCompId c
            return $ pruneC $ S.insert cid q

multFirst :: Points -> Int -> Int -> Int
multFirst a i j = a1 * b1
    where
        (P3 a1 _ _) = a A.! i
        (P3 b1 _ _) = a A.! j

part1 :: String -> IO ()
part1 fname = do
    contents <- readFile fname
    let rawPts = readPoints contents
        (_,n) = A.bounds rawPts
    (pts, (i,j)) <- aggregate n $ findMinPairs $ rawPts
    maxComps <- findMaxComponents pts
    putStrLn $ show $ S.size maxComps
    putStrLn $ show $ S.fold (\(CompId _ sz _) acc -> acc * sz) 1 maxComps

part2 :: String -> IO ()
part2 fname = do
    contents <- readFile fname
    let rawPts = readPoints contents
        (_,n) = A.bounds rawPts
    (pts, (i,j)) <- aggregate n $ findMinPairs $ rawPts
    putStrLn $ show $ multFirst rawPts i j
    -- maxComps <- findMaxComponents pts
    -- putStrLn $ show $ S.size maxComps
    -- putStrLn $ show $ S.fold (\(CompId _ sz _) acc -> acc * sz) 1 maxComps