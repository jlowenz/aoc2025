{-# LANGUAGE BangPatterns #-}

module Lib where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import qualified Data.Array as A
import qualified Data.IntervalMap.FingerTree as I

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Point2 = P2 Int Int
    deriving (Eq, Show)
type Points = A.Array Int Point2

origin :: Point2
origin = P2 0 0

class TileArea a where 
    area :: a -> Integer 

data Rect = Rect {
    rArea :: Integer,
    rectUL :: Point2,
    rectLR :: Point2,
    ptPair :: (Int,Int)
}
    deriving (Eq, Show)

instance Ord Rect where
    compare Rect{rArea=r1} Rect{rArea=r2} = compare r1 r2

instance TileArea Rect where
    area (Rect {rectUL = (P2 x1 y1), rectLR = (P2 x2 y2)}) = 
        toInteger (y2-y1+1) * toInteger (x2-x1+1) 

emptyRect :: Rect
emptyRect = Rect {rArea = 0, rectUL = origin, rectLR = origin, ptPair=(-1,-1)}

makeRect :: Int -> Point2 -> Int -> Point2 -> Rect
makeRect i (P2 x1 y1) j (P2 x2 y2) = r {rArea = (area r)}
    where 
        r = Rect {
            rArea = 0, 
            rectUL = P2 (min x1 x2) (min y1 y2),
            rectLR = P2 (max x1 x2) (max y1 y2),
            ptPair = (i,j)}

contains :: Rect -> Point2 -> Bool
contains (Rect {rectUL = (P2 x1 y1), rectLR = (P2 x2 y2)}) (P2 x y) = x1 <= x && x <= x2 && y1 <= y && y <= y2

isAnchor :: Rect -> Int -> Bool
isAnchor (Rect _ _ _ (i,j)) k = i == k || j == k

toPoint :: [String] -> Maybe Point2 
toPoint [] = Nothing
toPoint [sx,sy] = Just $ P2 (read sx) (read sy)
toPoint _ = Nothing

readPoints :: String -> Points
readPoints contents = A.listArray (1,n) ptList
    where 
        ptList = mapMaybe (toPoint . splitOn ",") $ lines contents
        n = length ptList

findMaxRect :: Points -> Rect
findMaxRect pts = go emptyRect [(i,j) | i <- [lo..hi], j <- [i+1..hi]]
    where
        (lo,hi) = A.bounds pts
        go r [] = r
        go r ((i,j):rest) = case (contains r pi, contains r pj) of
            (True,True) -> go r rest -- if both points contained, cannot be larger
            _ -> go (max r newr) rest
            where
                pi = pts A.! i
                pj = pts A.! j
                newr = (makeRect i pi j pj)

-- make the r/g polygon
data SegDir = ToLeft | ToRight | ToUp | ToDown -- not sure i need the directions
    deriving (Eq, Ord, Show)
data Seg = 
    Seg {common :: Int, lo :: Int, hi :: Int, dir :: SegDir }
    deriving (Show, Eq)

makeHSeg :: Int -> Int -> Int -> Seg
makeHSeg common x1 x2 = if x1 < x2
    then Seg common x1 x2 ToDown
    else Seg common x2 x1 ToUp

makeVSeg :: Int -> Int -> Int -> Seg
makeVSeg common y1 y2 = if y1 < y2 
    then Seg common y1 y2 ToLeft
    else Seg common y2 y1 ToRight

rectVSegs :: Rect -> [Seg]
rectVSegs (Rect _ (P2 xmin ymin) (P2 xmax ymax) _) = 
    [makeVSeg xmin ymax ymin, makeVSeg xmax ymin ymax]

rectHSegs :: Rect -> [Seg]
rectHSegs (Rect _ (P2 xmin ymin) (P2 xmax ymax) _) =
    [makeHSeg ymin xmin xmax, makeHSeg ymax xmax xmin]

-- Store the colinear segments in an interval map
type SegIntervals = I.IntervalMap Int Seg
-- A set of colinear segments
type SegMap = M.Map Int SegIntervals

data SegmentType = Horizontal | Vertical
newtype VSegs = VSegs SegMap deriving Show
newtype HSegs = HSegs SegMap deriving Show

emptyVSegs :: VSegs
emptyVSegs = VSegs M.empty
emptyHSegs :: HSegs
emptyHSegs = HSegs M.empty

data Polygon = Poly {
    lastPoint :: Maybe Point2,
    vsegs :: VSegs,
    hsegs :: HSegs }
    deriving (Show)

-- | Create an empty Polygon, with no segments
emptyPolygon :: Polygon
emptyPolygon = Poly { lastPoint = Nothing, vsegs = emptyVSegs, hsegs = emptyHSegs }

insertSeg :: SegMap -> Seg -> SegMap
insertSeg ss seg = case M.lookup c ss of
    Nothing -> M.insert c (I.singleton (I.Interval lo hi) seg) ss
    Just imap -> M.insert c (I.insert (I.Interval lo hi) seg imap) ss
    where 
        (Seg c lo hi _) = seg

findColinearIntersecting :: SegMap -> Seg -> [Seg]
findColinearIntersecting ss (Seg c lo hi _) =
    case M.lookup c ss of 
        Nothing -> []
        Just isegs -> snd $ unzip $ I.intersections (I.Interval lo hi) isegs 

findOrthogonalIntersecting :: SegMap -> Seg -> [Seg]
findOrthogonalIntersecting ss (Seg c lo hi _) =
    foldr findIntersecting [] csegs
    where
        csegs = fst $ M.split (hi+1) (snd $ M.split (lo-1) ss)
        findIntersecting iseg acc = acc ++ (snd $ unzip $ I.search c iseg)

-- Look up any colinear intersecting segments, 
-- ensure they have aligned direction. If none found, 
-- we'll just return true.
alignedWith :: SegMap -> Seg -> Bool
alignedWith ss seg@(Seg _ lo hi d) = all aligned coSegs
    where
        coSegs = findColinearIntersecting ss seg
        aligned s@(Seg _ lo' hi' d') = d == d' || lo == hi' || hi == lo'

-- ..............
-- .......#XXX#..
-- .......XXXXX..
-- ..OOOOOOOOXX..
-- ..OOOOOOOOXX..
-- ..OOOOOOOOXX..
-- .........XXX..
-- .........#X#..
-- ..............
-- hseg to v: c == hi && ToDown
-- hseg to v: c == lo && ToUp
-- vseg to h: c == lo && ToRight
-- vseg to h: c == hi && ToLeft 

badIntersect :: Seg -> Seg -> Bool 
badIntersect s@(Seg c lo hi d) s'@(Seg c' lo' hi' d') = 
    case d of 
        ToDown -> (lo < c' && c' < hi && hi' > c) ||
            (lo == lo' && d' == ToLeft) ||
            (hi == lo' && d' == ToRight) ||
            (lo == hi' && d' == ToRight) ||
            (hi == hi' && d' == ToLeft)
        ToUp -> (lo < c' && c' < hi && lo' < c) ||
            (lo == lo' && d' == ToRight) ||
            (hi == lo' && d' == ToLeft) ||
            (lo == hi' && d' == ToLeft) ||
            (hi == hi' && d' == ToRight)
        ToLeft -> (lo < c' && c' < hi && lo' < c) ||
            (lo == lo' && d' == ToDown) ||
            (hi == lo' && d' == ToUp) ||
            (lo == hi' && d' == ToUp) ||
            (hi == hi' && d' == ToDown)
        ToRight -> (lo < c' && c' < hi && hi' > c) ||
            (lo == lo' && d' == ToUp) ||
            (hi == lo' && d' == ToDown) ||
            (lo == hi' && d' == ToDown) ||
            (hi == hi' && d' == ToUp)


noCrossing :: SegMap -> Seg -> Bool
noCrossing ss seg@(Seg c _ _ d) = empty orthoSegs || all endPoints orthoSegs
    where
        orthoSegs = findOrthogonalIntersecting ss seg
        empty [] = True
        empty _ = False
        endPoints s@(Seg _ lo hi d') = not $ badIntersect seg s

inside :: SegMap -> Seg -> Bool
inside ss seg@(Seg c _ _ _) = odd crossings
    where
        crossings = length $ filter interior $ findOrthogonalIntersecting ss seg
        interior s@(Seg _ lo hi _) = lo /= c && hi /= c

addHSeg :: Polygon -> Point2 -> Int -> Polygon
addHSeg poly@(Poly {hsegs = (HSegs hs)}) p@(P2 x' y') x = poly { 
    lastPoint = Just p, 
    hsegs = HSegs (insertSeg hs $ makeHSeg y' x x')}

addVSeg :: Polygon -> Point2 -> Int -> Polygon
addVSeg poly@(Poly {vsegs = (VSegs vs)}) p@(P2 x' y') y = poly { 
    lastPoint = Just p, 
    vsegs = VSegs (insertSeg vs $ makeVSeg x' y y')}

segmentType :: Int -> Int -> SegmentType
segmentType _ 0 = Horizontal
segmentType 0 _ = Vertical
segmentType _ _ = error "Segment not vertical or horizontal"

addPoint :: Point2 -> Polygon -> Polygon
addPoint p@(P2 x' y') poly = case lastPoint poly of
    Nothing -> poly { lastPoint = Just p }
    Just (P2 x y) -> case segmentType (x' - x) (y' - y) of
        Horizontal -> addHSeg poly p x
        Vertical -> addVSeg poly p y 

buildPolygon :: Points -> Polygon
buildPolygon pts = go ([hi]++[lo..hi]) emptyPolygon
    where 
        (lo,hi) = A.bounds pts
        go [] poly = poly
        go (i:is) poly = go is (addPoint pi poly)
            where
                pi = pts A.! i

vsegsInPoly :: Polygon -> [Seg] -> Bool
vsegsInPoly _ [] = True
vsegsInPoly p@(Poly {vsegs = (VSegs vs), hsegs = (HSegs hs)}) (x:xs) =
    alignedWith vs x && noCrossing hs x && vsegsInPoly p xs

hsegsInPoly :: Polygon -> [Seg] -> Bool
hsegsInPoly _ [] = True
hsegsInPoly p@(Poly {vsegs = (VSegs vs), hsegs = (HSegs hs)}) (x:xs) =
    alignedWith hs x && noCrossing vs x && hsegsInPoly p xs

centerline :: Rect -> Seg
centerline (Rect _ (P2 x y) (P2 x' y') _) = Seg (y + div (y'-y) 2) 0 (x + div (x'-x) 2) ToUp

rectInPoly :: Polygon -> Rect -> Bool 
rectInPoly poly@(Poly{vsegs = (VSegs vs)}) r = 
    vsegsInPoly poly (rectVSegs r) &&
    hsegsInPoly poly (rectHSegs r) &&
    inside vs (centerline r) 

findMaxRectInPolygon :: Polygon -> Points -> Rect
findMaxRectInPolygon !poly !pts = go emptyRect [(i,j) | i <- [lo..hi], j <- [i+1..hi]]
    where
        (lo,hi) = A.bounds pts
        go r [] = r
        go r ((i,j):rest) = case (rectInPoly poly newr, rArea newr > rArea r) of
            (True,True) -> go newr rest
            _ -> go r rest
            where
                pi = pts A.! i
                pj = pts A.! j
                newr = (makeRect i pi j pj)

part1 :: String -> IO Rect
part1 fname = do 
    contents <- readFile fname
    let pts = readPoints contents
    return $ findMaxRect pts


part2 :: String -> IO Rect
part2 fname = do 
    contents <- readFile fname
    let pts = readPoints contents
        poly = buildPolygon pts
    return $ findMaxRectInPolygon poly pts
