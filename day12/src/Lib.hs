{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}


module Lib where

import GHC.Stack (HasCallStack)

import           Codec.Picture
import           Data.Sixel

import           Data.List.Split (splitOn)
import           Data.List (intersperse, sort, nub)
import qualified Data.Array.IArray as A
import qualified Data.Set as S 
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import           Debug.Trace (trace)

import Control.Monad
import Control.Monad.State.Strict 

type Coord = (Int,Int)
type CoordList = [Coord]
type IArray2d a = A.Array Coord a
type Array2d = A.Array Coord Bool
type Variants = A.Array Int Array2d

data Shape = S {
    idx :: !Int,
    variants :: !Variants,
    frontiers :: !(A.Array Int CoordList),
    occupancy :: !(A.Array Int CoordList),
    count :: !Int } 
    deriving Eq

variant :: Shape -> Int -> Array2d
variant sh i = sh.variants A.! i

frontier :: Shape -> Int -> CoordList
frontier sh i = sh.frontiers A.! i

occupancies :: Shape -> Int -> CoordList
occupancies sh i = sh.occupancy A.! i

-- | Shape instance is a particular rotation of a shape
data ShapeInst = SI Shape Int
    deriving Eq

instance Show ShapeInst where
    show (SI sh i) = (show $ variant sh i) ++ "\n"

allInstances :: Shape -> [ShapeInst]
allInstances sh = map (SI sh) [lo..hi]
    where
        (lo,hi) = A.bounds sh.variants

arrayFor :: ShapeInst -> Array2d
arrayFor (SI sh i) = variant sh i

shapeIndex :: ShapeInst -> Int
shapeIndex (SI sh _) = sh.idx

-- data MergeShape = 
--     MS { shapes :: [MergeShape], msWidth :: Int, msHeight :: Int, msWasted :: Int } |
--     SH { shape :: ShapeInst }

black   = PixelRGB8 0 0 0
white   = PixelRGB8 255 255 255
red     = PixelRGB8 255 80 80
green   = PixelRGB8 80 255 80
blue    = PixelRGB8 80 80 255
yellow  = PixelRGB8 255 255 80
cyan    = PixelRGB8 80 255 255
magenta = PixelRGB8 255 80 255
grey    = PixelRGB8 180 180 180
colors = [red, green, blue, yellow, magenta, cyan, grey]
numColors = length colors

array2dToPixel :: PixelRGB8 -> Array2d -> Int -> Int -> PixelRGB8
array2dToPixel color arr x y = if A.inRange bnds c && arr A.! c 
    then color
    else black
    where 
        bnds = A.bounds arr
        c = (y `div` 2, x `div` 2)

shapeToPixel :: Shape -> Int -> Int -> PixelRGB8
shapeToPixel sh x y = if mod (div x 6) 2 == 0 && A.inRange varBounds idx
        then case (sh.variants A.! idx) A.! (y `div` 2, x `mod` 6 `div` 2) of
            True -> colors !! idx
            False -> black
        else black
    where
        varBounds = A.bounds sh.variants
        idx = (x `div` 12)


shapeToImage :: Shape -> Image PixelRGB8
shapeToImage sh = generateImage (shapeToPixel sh) width 6 
    where
        numShapes = 1 + (snd $ A.bounds sh.variants)
        w = 6 * (numShapes * 2 - 1)
        width = max w 32

showShape :: Shape -> IO ()
showShape = putSixel . shapeToImage

array2dToImage :: Array2d -> Image PixelRGB8
array2dToImage arr = generateImage (array2dToPixel green arr) w h
    where
        w = max (width arr * 2) (32)
        h = height arr * 2

array2dToAscii :: Array2d -> String
array2dToAscii arr = go (0,0) ""
    where 
        (_, (maxY, maxX)) = A.bounds arr
        toStr :: Coord -> String
        toStr c = if arr A.! c then "#" else "."
        go :: Coord -> String -> String 
        go c@(y,x) s 
            | y > maxY = s
            | x <= maxX = go (y,x+1) (s ++ toStr c)
            | x > maxX = go (y+1,0) (s ++ "\n")

instance {-# Overlapping #-} Show Array2d where
    show = show . toSixel . array2dToImage

instance Show Shape where
    show = show . toSixel . shapeToImage 

type Shapes = A.Array Int Shape
type Inventory = A.Array Int Int

emptyInventory :: Inventory
emptyInventory = A.listArray (0,5) (replicate 6 0)

modifyInventory :: Inventory -> [(Int,Int)] -> Inventory
modifyInventory inv deltas = inv A.// (map f deltas)
    where
        f (i, d) = (i, (inv A.! i) + d)

incrementInventory :: Inventory -> Int -> Inventory
incrementInventory inv idx = modifyInventory inv [(idx, 1)]

decrementInventory :: Inventory -> Int -> Inventory
decrementInventory inv idx = modifyInventory inv [(idx, -1)]


instance {-# Overlapping #-} Show Inventory where
    show = show . A.elems

instance {-# Overlapping #-} Show (Shapes) where
    show sh = "\n" ++ (concat $ intersperse "\n" shapes) ++ "\n"
        where
            shapes :: [String]
            shapes = map show $ A.elems sh


data Region = R {
    regionWidth :: Int,
    regionHeight :: Int,
    inventory :: Inventory } 

type Limits = (Int,Int)

data Rect = Rect {
    rectMin :: Coord,
    rectMax :: Coord
} deriving (Eq, Show)

rectFor :: IArray2d a -> Rect
rectFor arr = Rect min max
    where
        (min, max) = A.bounds arr

regionRect :: Region -> Rect
regionRect (R w h _) = Rect (0,0) (h-1,w-1)

class Dim2d a where 
    width :: a -> Int
    height :: a -> Int
    area :: a -> Int
    area item = (width item) * (height item)

class HasCost a where
    cost :: a -> Float
    headCost :: [a] -> Float
    headCost [] = 0.0
    headCost (x:xs) = cost x

{--
min(0,0)
max(2,1)
. . .
. . .
--}

instance Dim2d Rect where
    width (Rect (xmin,_) (xmax,_)) = xmax - xmin + 1
    height (Rect (_,ymin) (_,ymax)) = ymax - ymin + 1

instance Dim2d (IArray2d a) where
    width arr = y + 1
        where (_,y) = (snd $ A.bounds arr)
    height arr = x + 1
        where (x,_) = (snd $ A.bounds arr)

data Placement = Place {
    origin :: !Coord,
    shapeInst :: !ShapeInst
} deriving (Eq)

instance Show Placement where 
    show (Place origin (SI sh i)) = "(" ++ show origin ++ "," ++ show sh.idx ++ "." ++ show i ++ ")"

data Packing = Pack {
    packRect :: !Rect,
    packArea :: !Int,
    packInventory :: !Inventory, -- the inventory used for this packing
    packUtilization :: !Float, -- fraction of used area 0.0 < u <= 1.0, 1.0 is best
    packWaste :: !Int, -- how many units are unoccupied, but enclosed?
    packItems :: ![Placement], -- every shape and location
    packArray :: !Array2d, -- occupancy for this packing
    packFrontier :: !(S.Set Coord) -- keep track of the unoccupied coords
} deriving (Eq)

instance Show Packing where
    show p = concat $ intersperse "\n" ["Packing:", 
        " rect: " ++ show p.packRect,
        " area: " ++ show p.packArea, 
        " inventory: " ++ show p.packInventory, 
        " utilization: " ++ show p.packUtilization,
        " waste: " ++ show p.packWaste,
        " items: " ++ show p.packItems,
        " array: " ++ show p.packArray, "\n"]

instance HasCost Packing where
    cost p = waste + (10.0 * (1.0 - p.packUtilization)) + area
        where
            waste = realToFrac p.packWaste
            area = realToFrac p.packArea

class ToPacking a where
    toPacking :: a -> Packing

newtype MinimalPacking = MinP Packing
    deriving Eq
newtype MaximalPacking = MaxP Packing
    deriving Eq

instance ToPacking MinimalPacking where
    toPacking (MinP p) = p

instance HasCost MinimalPacking where
    cost (MinP p) = cost p

instance ToPacking MaximalPacking where
    toPacking (MaxP p) = p

instance Dim2d Packing where
    width p = width p.packRect
    height p = height p.packRect

{-- 
minimal incremental packing
- low waste
- small area
- high utilization
--}
instance Ord MinimalPacking where 
    compare (MinP p1) (MinP p2) = compare p1c p2c
        where
            p1c = (p1.packArea, p1.packWaste, 1.0 - p1.packUtilization)
            p2c = (p2.packArea, p2.packWaste, 1.0 - p2.packUtilization)

translateCoords :: Coord -> (Coord,a) -> (Coord,a)
translateCoords (x,y) ((ix,iy),it) = ((ix+x,iy+y),it)

translate2dAssocs :: IArray2d a -> Coord -> [(Coord,a)]
translate2dAssocs arr c = map (translateCoords c) (A.assocs arr)

-- | Does the first array fit into the second array at the coordinate?
shapeFits :: Array2d -> Coord -> Array2d -> Bool
shapeFits sh offset arr = all id $ map (fits arr) els
    where
        els = map (translateCoords offset) (A.assocs sh)
        fits :: Array2d -> (Coord,Bool) -> Bool
        fits arr (q, b) = if A.inRange bnds q 
            then case (arr A.! q, b) of 
                    (True,True) -> False
                    otherwise -> True
            else True
            where bnds = A.bounds arr

{--

(0,0)
* * * * 
* * * *
* * * * (3,2)

(0,0)
* * * * .
* * * * *
* * * * * 
. . * * * (4,3)

c = (2,1)
b = (2,2)
--}


-- | Returns a copy of arr with new bounds, any new elements filled with the default value.
resizeArray2d :: IArray2d a -> a -> Coord -> IArray2d a
resizeArray2d arr defaultVal newHi = A.array newBnds newVals
    where
        (newI, newJ) = newHi
        bnds@((iLo,jLo), (iHi,jHi)) = A.bounds arr
        newBnds = ((iLo,jLo), newHi)
        newVals = [((i,j),v) | i <- [iLo..newI], j <- [jLo..newJ], v <- [getV arr (i,j)]]
        -- getV :: IArray2d a -> Coord -> a
        getV a c = if A.inRange bnds c then a A.! c else defaultVal


coordmax :: Coord -> Coord -> Coord
coordmax (x1,y1) (x2,y2) = (max x1 x2, max y1 y2)

coordPlus :: Coord -> Coord -> Coord
coordPlus (x1,y1) (x2,y2) = (x1+x2, y1+y2)

coordMinus :: Coord -> Coord -> Coord
coordMinus (x1,y1) (x2,y2) = (x1-x2,y1-y2)

translateCoordList :: Coord -> CoordList -> CoordList
translateCoordList c coords = map (coordPlus c) coords

-- type OccupancyAssocs = [(Coord,Bool)]

-- mergeAssocs :: Array2d -> OccupancyAssocs -> Array2d
-- mergeAssocs arr occs = foldr f arr occs
--     where
--         f :: (Coord,Bool) -> Array2d -> Array2d
--         f occ@(c,b) arr = if b then arr A.// [occ]

-- | Merge the first array into the second, expanding bounds if necessary
copyInto :: Array2d -> Coord -> Array2d -> Array2d
copyInto src c dst = out A.// els
    where
        els = filter snd $ translate2dAssocs src c
        (_, srcHiBnds) = A.bounds src
        (_, dstHiBnds) = A.bounds dst
        newBnds = coordmax (coordPlus c srcHiBnds) dstHiBnds
        out = resizeArray2d dst False newBnds

-- countOccupiedUnits :: Array2d -> Int
-- countOccupiedUnits = sum . map f . A.elems
--     where
--         f !b = if b then 1 else 0

countOccupiedUnits :: Array2d -> Int
countOccupiedUnits = A.foldlArray' f 0
    where
        f b e = b + (if e then 1 else 0)


occupied :: (a,Bool) -> Bool
occupied = snd

unoccupied :: (a,Bool) -> Bool
unoccupied = not . occupied

unoccupiedUnits :: Array2d -> [(Coord, Bool)]
unoccupiedUnits = filter unoccupied . A.assocs

utilization :: Array2d -> Float
utilization arr = (realToFrac $ countOccupiedUnits arr) / (realToFrac $ area arr)

type Visited = S.Set Coord
type ExposedCache = M.Map Coord Bool -- True if Coord is exposed, False otherwise

neighbors :: Coord -> [Coord]
neighbors (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

recordExposed :: Coord -> Bool -> State ExposedCache Bool
recordExposed c b = do
    cache <- get
    put $ M.insert c b cache
    return b

checkExposed :: Coord -> State ExposedCache (Maybe Bool)
checkExposed c = do
    cache <- get 
    return $ M.lookup c cache

setInsert :: Ord a => [a] -> S.Set a -> S.Set a
setInsert [] s = s
setInsert (x:xs) s = setInsert xs (S.insert x s)

exposed :: Array2d -> Coord -> State ExposedCache Bool
exposed a coord = if a A.! coord then return False else do
    mExposed <- checkExposed coord
    case mExposed of 
        Nothing -> go S.empty (S.fromList (neighbors coord))
        Just m -> return m
    where
        bnds = A.bounds a
        go :: Visited -> S.Set Coord -> State ExposedCache Bool
        -- go _ coords
        --     | S.minView coords -> Nothing = do 
        --         recordExposed coord False -- we never found a boundary
        go v coords  
            | S.minView coords == Nothing = do
                recordExposed coord False
            | not (A.inRange bnds c) = let (y,x) = c in 
                -- if the oob occurs top or left, then we don't say it's exposed yet
                if y < 0 || x < 0 then go (S.insert c v) cs else do 
                    recordExposed coord True -- out of bounds, so exposed
            | S.member c v = go v cs -- already visited before, continue search
            | a A.! c = go (S.insert c v) cs -- occupied cell, continue search
            | otherwise = do 
                mExposed' <- checkExposed c 
                case mExposed' of 
                    Nothing -> do 
                        e <- go (S.insert c v) (setInsert (neighbors c) cs)
                        recordExposed c e
                    Just m -> return m
            where 
                (c, cs) = S.deleteFindMin coords


-- | Idea: do dfs for every unoccupied unit - 
-- if boundary found, NOT wasted
-- if boundary NOT found, wasted
wastedUnits :: Array2d -> Int
wastedUnits arr = evalState (go (unoccupiedUnits arr)) M.empty
    where
        go :: [(Coord,Bool)] -> State ExposedCache Int
        go unocc = do
            counts <- mapM f (fst $ unzip unocc) 
            return $ sum counts
        f :: Coord -> State ExposedCache Int
        f c = do 
            isExp <- exposed arr c
            return $ if isExp then 0 else 1

unoccupiedInRegion :: Array2d -> (Coord, Coord) -> [Coord]
unoccupiedInRegion arr ((iLo, jLo), (iHi, jHi)) = unocc
    where
        coords = [(i,j) | i <- [iLo..iHi], j <- [jLo,jHi]]
        unocc = mapMaybe f coords
        f c = if not $ arr A.! c then Just c else Nothing

wastedUnits2 :: Array2d -> (Coord,Coord) -> Int
wastedUnits2 arr bnds = evalState (go coords) M.empty
    where
        coords = unoccupiedInRegion arr bnds
        go :: [Coord] -> State ExposedCache Int
        go unocc = do
            counts <- mapM f unocc 
            return $ sum counts
        f :: Coord -> State ExposedCache Int
        f c = do 
            isExp <- exposed arr c
            return $ if isExp then 0 else 1

{--
####
####
##..
####
    (3,3)

limit = (6,6) (width,height)
--}    

-- Expands each (x,y) to the 4 coords starting (x-1,y-1)..(x,y)
expandUL :: [Coord] -> [Coord]
expandUL coords = concat $ map expand coords
    where 
        expand (x,y) = [(i,j) | i <- [x-1..x], j <- [y-1..y]]

-- Use a set to remove duplicates
nubSet :: Ord a => [a] -> [a]
nubSet = go S.empty
    where
        go s cs = S.elems $ foldl' (flip S.insert) s cs

candidateLocations :: Array2d ->  Limits -> [Coord]
candidateLocations arr (maxW, maxH) = coords
    where
        stdShapeSize = (2,2)
        (lo, upperBnd@(h,w)) = A.bounds arr
        maxBnds = ((0,0),coordMinus (maxH-1,maxW-1) stdShapeSize)
        interior = expandUL $ fst $ unzip $ unoccupiedUnits arr
        exterior = [(h+1,j) | j <- [0..w]] ++ [(i,w+1) | i <- [0..h]] ++ [(h+1,w+1)]
        coords = filter (A.inRange maxBnds) $ nubSet $ concat [interior, exterior]

candidateLocations2 :: Packing -> Limits -> CoordList
candidateLocations2 p (maxW, maxH) = filter inBnds (S.elems p.packFrontier)
    where
        stdShapeBnd = (2,2)
        maxBnds = ((0,0), coordMinus (maxH-1,maxW-1) stdShapeBnd)
        inBnds c = A.inRange maxBnds c

packingFromShape :: ShapeInst -> Packing
packingFromShape shInst@(SI sh i) = Pack {
    packRect = rectFor arr,
    packArea = area arr,
    packInventory = incrementInventory emptyInventory sh.idx,
    packUtilization = utilization arr,
    packWaste = wastedUnits arr,
    packItems = [Place (0,0) shInst],
    packArray = arr,
    packFrontier = S.fromList $ frontier sh i } 
    where 
        arr = variant sh i

type FrontierSet = S.Set Coord 

deleteList :: Ord a => S.Set a -> [a] -> S.Set a 
deleteList s items = foldr S.delete s items

insertList :: Ord a => S.Set a -> [a] -> S.Set a
insertList s items = foldr S.insert s items 

-- updates the frontier for A SINGLE ADDED SHAPE at c 
updateFrontier :: FrontierSet -> Coord -> ShapeInst -> Array2d -> FrontierSet
updateFrontier fs c (SI sh i) arr = insertList (deleteList fs shOcc) validFrontier
    where
        tr = translateCoordList c
        shOcc = tr $ occupancies sh i
        shFront = tr $ frontier sh i
        bnds = A.bounds arr
        valid at = ((A.inRange bnds at) && (not $ arr A.! at)) || (at >= (0,0))
        validFrontier = filter valid shFront

updateWasted :: Int -> Array2d -> Coord -> Int
updateWasted oldW arr c@(i,j) = oldW + newW
    where
        loBnd = (max (i-1) 0, j)
        hiBnd = (i+2, j+2)
        newW = wastedUnits2 arr (loBnd,hiBnd)

-- Makes a new packing from the given packing and a shape instance at coord
makePacking :: HasCallStack => Packing -> ShapeInst -> Coord -> Packing
makePacking p shInst c = Pack {
    packRect = newRect,
    packArea = area newArray,
    packInventory = incrementInventory p.packInventory (shapeIndex shInst),
    packUtilization = utilization newArray,
    packWaste = updateWasted p.packWaste newArray c,
    packItems = Place c shInst : p.packItems,
    packArray = newArray,
    packFrontier = newFrontier }
    where 
        sh = arrayFor shInst
        newArray = copyInto sh c p.packArray
        (min, max) = A.bounds newArray
        newRect = Rect min max
        newFrontier = updateFrontier p.packFrontier c shInst newArray


-- headCost :: [Packing] -> Float
-- headCost [] = 0.0
-- headCost (x:xs) = cost x

bestCost :: (Ord a, HasCost a) => [a] -> [a]
bestCost xs = takeWhile (\x -> cost x == minCost) orderedXs
    where
        orderedXs = sort xs
        minCost = headCost orderedXs

-- take unordered list of Packings, return the "best"
bestPackings :: [Packing] -> [Packing]
bestPackings ps = map toPacking $ bestCost $ map MinP ps

initialShapePackings :: Shape -> [Packing] 
initialShapePackings sh = map packingFromShape $ allInstances sh

generatePackings :: Packing -> ShapeInst -> [Coord] -> [Packing]
generatePackings p shInst coords = bestPackings ps
    where
        sh = arrayFor shInst
        fits c = if shapeFits sh c p.packArray then Just c else Nothing
        fittings = mapMaybe fits coords
        ps = map (makePacking p shInst) fittings

-- | Compute a new packing for a shape from an existing packing and given limits
-- Returns 0 or more Packings: 0 if no configuration of shape can be packed within the limits
-- or 1+ if there is 1 or more packings with the same minimal score
-- for each variant
-- . generate a list of packings
packShape :: Packing -> Limits -> Shape -> [Packing]
packShape p limits sh = go
    where
        -- coords = candidateLocations2 p limits
        coords = candidateLocations p.packArray limits
        go = concat $ map (\inst -> generatePackings p inst coords) (allInstances sh) 

nextAvailableShapes :: Shapes -> Inventory -> [Shape]
nextAvailableShapes shps inv = mapMaybe f $ A.assocs inv
    where
        f :: (Int,Int) -> Maybe Shape
        f (i,c) = if c > 0 then Just (shps A.! i) else Nothing

data PackingSearch = Search {
    currPacking :: Packing,
    remInventory :: Inventory,
    searchRect :: Rect -- maybe this should be limits
} deriving Eq

instance HasCost PackingSearch where
    cost ps = cost ps.currPacking

instance Ord PackingSearch where
    compare (Search p1 _ _) (Search p2 _ _) = compare (MinP p1) (MinP p2)

instance Show PackingSearch where
    show ps = "Search: " ++ (show ps.remInventory) 
        ++ ", " ++ (show ps.searchRect) ++ "\n" ++ (show ps.currPacking)

rectToLimits :: Rect -> Limits 
rectToLimits r = (x+1, y+1) 
    where 
        (y, x) = r.rectMax

stepSearchWithShape :: PackingSearch -> Shape -> [PackingSearch]
stepSearchWithShape (Search origP inv r) sh = if invHasShape 
    then bestCost $ map (\p -> Search p newInv r) $ packShape origP limits sh
    else []
    where 
        limits = rectToLimits r 
        newInv = decrementInventory inv sh.idx
        invHasShape = inv A.! sh.idx > 0


packingSearchFromShape :: Rect -> Inventory -> Shape -> [PackingSearch]
packingSearchFromShape r inv sh = bestCost $ map (\p -> Search p newInv r) $ initialShapePackings sh
    where 
        newInv = decrementInventory inv sh.idx -- commmon for all searches

initializeSearch :: Shapes -> Region -> [PackingSearch]
initializeSearch shps region@(R _ _ inv) = bestCost $ initSearches
    where
        r = regionRect region
        initSearches = concat $ map (packingSearchFromShape r inv) $ nextAvailableShapes shps inv

noMoreInventory :: Inventory -> Bool
noMoreInventory inv = all (\s -> s == 0) $ A.elems inv

simpleCheck :: Shapes -> Region -> Maybe Bool
simpleCheck shps (R w h inv) = if simpleFit then Just True else 
        if tooBig then Just False else Nothing
    where
        total3x3 = (w `div` 3) * (h `div` 3) * 9
        simpleFit = (A.foldrArray' (\e acc -> acc + (e * 9)) 0 inv) <= total3x3
        tooBig = (foldl' f 0 (A.assocs inv)) > (w*h)
        f acc (i,nPres) = acc + (nPres * occ)
            where 
                occ = count (shps A.! i)

canPackRegion :: Shapes -> Region -> Bool 
canPackRegion shps region = case simpleCheck shps region of
    Just b -> b
    Nothing -> canPackRegion' shps region

canPackRegion' :: Shapes -> Region -> Bool 
canPackRegion' shps region = go (initializeSearch shps region)
    where
        go :: [PackingSearch] -> Bool
        go [] = trace "False\n" False -- couldn't use all inventory
        go (ps:pss) = if noMoreInventory ps.remInventory --(trace (show ps) $ noMoreInventory ps.remInventory)
        -- go (ps:pss) = if (trace (show ps) $ noMoreInventory ps.remInventory)
            then trace "True\n" True
            else go $ (bestCost $ concat $ map (stepSearchWithShape ps) nextShapes) ++ pss
            where 
                nextShapes = nextAvailableShapes shps ps.remInventory
                

instance Show Region where
    show (R w h inv) = show w ++ "x" ++ show h ++ ": " ++ (concat $ intersperse " " items)
        where
            items :: [String]
            items = map show $ A.elems inv

type Regions = [Region]

data Problem = P {
    shapes :: Shapes,
    regions :: Regions } 
    deriving Show

getShape :: Problem -> Int -> Int -> Array2d
getShape p i v = variant (p.shapes A.! i) v

rotateShape :: Int -> Array2d -> Array2d
rotateShape 0 arr = arr
rotateShape n arr = rotateShape (n-1) $ A.array ((0,0),(2,2)) $ zip to (A.elems arr)
    where 
        to = [(2,0),(1,0),(0,0),(2,1),(1,1),(0,1),(2,2),(1,2),(0,2)]

makeFrontier :: Array2d -> CoordList
makeFrontier arr = nubSet $ down ++ right ++ up ++ left
    where
        check dir c = if arr A.! (coordPlus c dir) then c else check dir (coordPlus c dir) 
        checkDown c = check (1,0) c
        checkRight c = check (0,1) c
        checkUp c = check (-1,0) c 
        checkLeft c = check (0,-1) c
        interior = [0..2]
        down = map checkDown [(-1,j) | j <- interior]
        right = map checkRight [(i,-1) | i <- interior]
        up = map checkUp [(3,j) | j <- interior]
        left = map checkLeft [(i,3) | i <- interior]

makeOccs :: Array2d -> CoordList
makeOccs arr = map fst $ filter occupied $ A.assocs arr

makeShape :: Int -> Array2d -> Shape
makeShape i shp = S { idx = i, 
    variants = A.listArray (0,n-1) shapes,  
    frontiers = A.listArray (0,n-1) fronts, 
    occupancy = A.listArray (0,n-1) occs, 
    count = length $ filter occupied $ A.assocs shp }
    where
        shapes = nubSet $ [shp] ++ [rotateShape v shp | v <- [1..3]]
        n = length shapes
        fronts = map makeFrontier shapes
        occs = map makeOccs shapes

parseShape :: Int -> [String] -> Shape
parseShape ix lns = makeShape ix $ A.array ((0,0),(2,2)) $ go $ concat (take 3 (drop 1 lns))
    where
        go :: String -> [((Int,Int),Bool)]
        go s = zip [(i,j) | i <- [0..2], j <- [0..2]] $ map (\c -> c == '#') s 

parseShapes :: [String] -> Shapes
parseShapes ls = A.array (0,5) $ go 0 ls
    where
        go :: Int -> [String] -> [(Int,Shape)]
        go _ [] = []
        go i allLines = (i, parseShape i $ take 5 allLines) : go (i+1) (drop 5 allLines)

readInt :: String -> Int
readInt = read

parseRegion :: String -> Region
parseRegion s = R w h inv
    where
        (dims:invStr:_) = splitOn ":" s
        (w:h:_) = (map read $ splitOn "x" dims) :: [Int]
        inv = A.array (0,5) $ zip [0..5] $ map readInt $ words invStr

parseRegions :: [String] -> Regions
parseRegions [] = []
parseRegions (s:ss) = parseRegion s : parseRegions ss

parseInput :: String -> Problem
parseInput contents = P (parseShapes shapeLines) (parseRegions regionLines)
    where
        allLines = lines contents
        shapeLines = take 30 allLines
        regionLines = drop 30 allLines 

loadProblem :: FilePath -> IO Problem
loadProblem fname = do 
    contents <- readFile fname
    return $ parseInput contents

toInt :: Bool -> Int
toInt True = 1
toInt False = 0

part1 :: FilePath -> IO ()
part1 fname = do
    p <- loadProblem fname
    print p
    let packs = map (canPackRegion p.shapes) p.regions
    print (sum $ map toInt packs)

{--
Principles: 
- constrain the search 
- minimize voids 
--}