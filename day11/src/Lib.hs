{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib (
    someFunc, buildGraph, stringToNode, nodeToString, countPaths, part1, part2, testPart1, test, doDfs, loadGraph) 
    where

import Debug.Trace (trace)
import Data.Bits 
import Data.Char (ord, chr)
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe, fromJust, fromMaybe)
import Data.MemoTrie (memo2)
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Set as S
import qualified Data.Map as M

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Our graph is represented as a map of Node -> OutNodes
type Node = Int
type OutNodes = [Node]
type NodeMap = M.Map Node OutNodes
data Graph = G {
    fwd :: NodeMap,
    rev :: NodeMap}
    deriving Show

-- Convert the node name (a string) to a node (int) for quicker
-- manipulation: 
stringToNode :: String -> Node
stringToNode = go 0 0 
    where
        go :: Int -> Int -> String -> Node
        go _ n [] = n
        go k n (x:xs) = go (k+1) newN xs
            where 
                newN = n+(shiftL (ord x) (8*k))

nodeToString :: Node -> String
nodeToString n = go n []
    where
        go :: Node -> String -> String
        go 0 out = reverse out
        go n out = go (shiftR n 8) (c:out)
            where 
                c = chr (n .&. 255)


emptyGraph :: Graph
emptyGraph = G M.empty M.empty

nodeCount :: Graph -> Int 
nodeCount (G m _) = M.size m

outNodes :: Graph -> Node -> Maybe OutNodes
outNodes (G m _) n = M.lookup n m 

revOutNodes :: Graph -> Node -> Maybe OutNodes
revOutNodes (G _ m) n = M.lookup n m

removeNodes :: Graph -> [Node] -> Graph
removeNodes g [] = g
removeNodes g@(G m r) (n:ns) = removeNodes (G (M.delete n m) (M.delete n r)) ns

buildGraph :: String -> Graph
buildGraph = go emptyGraph . lines
    where 
        go :: Graph -> [String] -> Graph
        go g [] = g
        go (G m r) (x:xs) = go (G (M.insert k vs m) (foldr (revInsert k) r vs)) xs
            where 
                parts = splitOn ":" x
                k = stringToNode (parts !! 0)
                vs = map stringToNode $ words (parts !! 1)
        revInsert :: Node -> Node -> NodeMap -> NodeMap
        revInsert v k m = case M.lookup k m of
            Just vs -> M.insert k (v:vs) m
            Nothing -> M.insert k [v] m

-- endNode :: Node 
-- endNode = stringToNode "out"

data Path = P { 
    visited :: S.Set Node,
    path :: [Node]}

instance Show Path where
    show (P _ path) = "Path: " ++ (show $ (map nodeToString path))

recentNode :: Path -> Node
recentNode (P _ (x:xs)) = x
recentNode _ = error "Not reachable"

newPath :: Node -> Path
newPath n = P (S.singleton n) [n]

pathLength :: Path -> Int
pathLength (P _ ps) = length ps

-- | Extends a path, returning (Just p) if p was extended, 
--   Nothing if a loop was found (cannot extend the path)
extendPath :: Path -> Node -> Maybe Path
extendPath p@(P visited path) n = case S.member n visited of
    -- True -> trace "Found loop" Nothing
    True -> Nothing
    -- False -> trace blah (Just $ P (S.insert n visited) (n:path))
    False -> Just $ P (S.insert n visited) (n:path)
    where 
        blah = "Extend " ++ (show $ nodeToString n) ++ " -> " ++ (show p) ++ " " ++ (show $ pathLength p)
-- extendPath p@(P visited path) n = Just $ P (S.insert n visited) (n:path)


data AllPaths = A {
    paths :: [Path]}
    deriving Show

emptyAllPaths :: AllPaths
emptyAllPaths = A []

pathCount :: AllPaths -> Int
pathCount (A ps) = length ps

outNode = (stringToNode "out")

bfsAllPaths :: Graph -> Node -> (Node -> Bool) -> (Path -> Bool) -> AllPaths
bfsAllPaths g@(G m r) start end f = go [newPath start] emptyAllPaths
    where
        go :: [Path] -> AllPaths -> AllPaths
        go [] a = a
        go ps (A paths) = go expanded (A (paths ++ foundPaths))
            where 
                nextPaths = filter nonTerminal ps -- (trace (show $ length ps) ps)
                foundPaths = filter f $ filter terminal ps
                expanded = concat $ map expandPaths nextPaths
        expandPaths :: Path -> [Path]
        expandPaths p = mapMaybe (extendPath p) outs
            where
                outs = case M.lookup (recentNode p) m of 
                    Just o -> o
                    Nothing -> []
        terminal :: Path -> Bool
        terminal p = term -- if term then trace "Term" term else term
            where 
                last = recentNode p
                term = (last == outNode) || (end last)
        nonTerminal :: Path -> Bool
        nonTerminal = not . terminal

bfsAllPathsRev :: Graph -> Node -> (Node -> Bool) -> (Path -> Bool) -> AllPaths
bfsAllPathsRev g@(G m r) start end f = go [newPath start] emptyAllPaths
    where
        go :: [Path] -> AllPaths -> AllPaths
        go [] a = a
        go ps (A paths) = 
            go (concat $ map expandPaths nextPaths) (A (paths ++ foundPaths))
            where 
                nextPaths = filter nonTerminal ps --(trace (show $ length ps) ps)
                foundPaths = filter f $ filter terminal ps
        expandPaths :: Path -> [Path]
        expandPaths p = mapMaybe (extendPath p) outs
            where
                outs = case M.lookup (recentNode p) r of 
                    Just o -> o
                    Nothing -> []
        terminal :: Path -> Bool
        terminal p = term --if term then trace "Term" term else term  
            where
                last = recentNode p
                term = (last == outNode) || (end last)
        nonTerminal :: Path -> Bool
        nonTerminal = not . terminal

-- dfsAllPaths :: Graph -> Node -> Node -> AllPaths
-- dfsAllPaths g@(G m r) start end = go (Just $ newPath start) emptyAllPaths
--     where
--         go :: Maybe Path -> AllPath -> AllPaths
--         go Nothing a = a
--         go (Just p) (A paths) = 
--             go (expandPath p )

part1 :: String -> IO ()
part1 fname = do
    contents <- readFile fname
    let g = buildGraph contents
        youNode = stringToNode "you"
        outNode = stringToNode "out"
    putStrLn $ show $ g
    putStrLn $ show $ nodeCount g
    let allPaths = bfsAllPaths g youNode (\n -> n == outNode) (const True)
    putStrLn $ show $ allPaths
    putStrLn $ show $ pathCount allPaths
    
dacAndFft :: Path -> Bool
dacAndFft (P visited _) = 
    S.member dac visited && S.member fft visited
    where 
        dac = stringToNode "dac"
        fft = stringToNode "fft"

dacOrFft :: Node -> Bool
dacOrFft n = 
    n == (stringToNode "dac") ||
    n == (stringToNode "fft")

lastn :: Node -> Path -> Bool
lastn n p = recentNode p == n

lastDac :: Path -> Bool
lastDac = lastn (stringToNode "dac")

lastFft :: Path -> Bool
lastFft = lastn (stringToNode "fft")

lastSvr :: Path -> Bool
lastSvr = lastn (stringToNode "svr")

lastOut :: Path -> Bool
lastOut = lastn (stringToNode "out")

collectNodes :: [Path] -> S.Set Node
collectNodes paths = go S.empty paths
    where
        go :: S.Set Node -> [Path] -> S.Set Node
        go s [] = s
        go s ((P _ pth):ps) = go (insertNodes s (tail pth)) ps
            where 
                insertNodes :: S.Set Node -> [Node] -> S.Set Node
                insertNodes s [] = s
                insertNodes s (n:ns) = insertNodes (S.insert n s) ns

{--
We need to do a topological-sort like algorithm to count the paths between
the fft and dac node (since we know svr to fft and dac to out). 

So, the main idea: we don't need to _enumerate_ all paths. We just need to count 
all the paths. So, if we start from the fft node, do a breadth first search and 
pass the in-counts 
--}

type InDegreeMap = M.Map Edge Integer

showIng ing = map (\((k1,k2),v) -> ((nodeToString k1, nodeToString k2), v)) $ M.toAscList ing

type Edge = (Node,Node)

next :: Graph -> [Edge] -> Node -> [Edge]
next (G m _) nodes excl = concat 
    $ map (\n -> toEdge n (M.lookup n m)) targets
    where 
        targets = filter (/=excl) $ nub $ snd $ unzip nodes
        toEdge :: Node -> Maybe [Node] -> [Edge]
        toEdge n (Just ns) = map ((,) n) (filter (/=excl) ns)
        toEdge n Nothing = []


edges :: Graph -> Node -> Node -> [Edge]
edges (G m _) n excl = map mkEdge $ filter (/=excl) $ fromJust (M.lookup n m)
    where
        mkEdge e = (n, e)

ways :: Graph -> Node -> Node -> Integer
ways g@(G m _) = memo2 $ \src dst -> 
    if src == dst then 1
    else sum [ways g nxt dst | nxt <- M.findWithDefault [] src m]

type Cache = S.Set (Int,Int) 

updateIng :: Graph -> Edge -> InDegreeMap -> InDegreeMap
updateIng g@(G _ r) e@(node,_) ing = M.insert e (foldr f 0 (M.findWithDefault [] node r)) ing
    where
        f :: Node -> Integer -> Integer
        f n acc = acc + (M.findWithDefault 0 (n,n) ing)

showEdge (a,b) = show $ (nodeToString a, nodeToString b)

countPaths :: Graph -> Integer -> Node -> Node -> Node -> Integer
countPaths g@(G m _) init start end excl = go (M.singleton (start,start) init) $ edges g start excl
    where
        go :: InDegreeMap -> [Edge] -> Integer
        go ing [] = fromMaybe 0 $ M.lookup (end,end) ing 
        go ing nodes = go (foldr accInDeg ing nodes) nextNodes
            where
                nextNodes = next g nodes excl
                f :: Integer -> Maybe Integer -> Maybe Integer
                f k ma = case ma of
                    Just a -> Just (k+a) 
                    Nothing -> Just k
                accInDeg :: Edge -> InDegreeMap -> InDegreeMap
                accInDeg e@(inNode,node) ing = case M.lookup (node,node) ing of
                    Just k -> updateIng g (node,node) ing
                    Nothing -> M.insert (node,node) (M.findWithDefault 1 (inNode,inNode) ing) ing

type Memo = M.Map Node Int

lookupMemo :: Node -> State Memo (Maybe Int)
lookupMemo node = do
  memo <- get
  return (M.lookup node memo)

writeMemo :: Node -> Int -> State Memo ()
writeMemo node dist = do
  memo <- get
  put (M.insert node dist memo)
  
dfs :: Graph -> Node -> Node -> State Memo Int
dfs graph@(G m _) node end 
  | node == end =
      return 1
  | otherwise = do
      optd <- lookupMemo node
      case optd of
        Just d -> return d
        Nothing -> do
          dists <- sequence [ dfs graph node' end
                            | node' <- fromMaybe []
                                       (M.lookup node m)
                            ]
          let dist = sum dists
          writeMemo node dist
          return dist

countPathsM :: Graph -> Integer -> Node -> Node -> Node -> InDegreeMap
countPathsM g@(G m _) init start end excl = go (M.singleton (start,start) init) $ edges g start excl
    where
        go :: InDegreeMap -> [Edge] -> InDegreeMap
        go ing [] = ing
        go ing nodes = go (foldr accInDeg ing nodes) nextNodes
            where
                nextNodes = next g nodes excl
                f :: Integer -> Maybe Integer -> Maybe Integer
                f k ma = case ma of
                    Just a -> Just (k+a) 
                    Nothing -> Just k
                accInDeg :: Edge -> InDegreeMap -> InDegreeMap
                accInDeg (inNode,node) ing = case M.lookup (inNode,inNode) ing of
                    Just k -> M.alter (f k) (node,node) ing
                    Nothing -> error "Error?"

loadGraph :: FilePath -> IO Graph
loadGraph fname = do
    contents <- readFile fname
    return $ buildGraph contents

part2 :: FilePath -> IO ()
part2 fname = do
    contents <- readFile fname
    let g = buildGraph contents
        svrNode = stringToNode "svr"
        outNode = stringToNode "out"
        dacNode = stringToNode "dac"
        youNode = stringToNode "you"
        fftNode = stringToNode "fft"
        remNodes = map stringToNode ["hlo", "czi", "qdk"]
    let --(A svrToFft) = bfsAllPaths g svrNode (\n -> n == fftNode) lastFft
        --(A dacToOut) = bfsAllPaths g dacNode (\n -> n == outNode) lastOut
        svrToFft = countPaths g 1 svrNode fftNode 0
        fftToDac = countPaths g 1 fftNode dacNode svrNode
        dacToOut = countPaths g 1 dacNode outNode svrNode
        svrToFft' = evalState (dfs g svrNode fftNode) M.empty
        fftToDac' = evalState (dfs g fftNode dacNode) M.empty
        dacToOut' = evalState (dfs g dacNode outNode) M.empty
        bfsSvrToFft = length $ paths $ bfsAllPathsRev g fftNode (\n -> n == svrNode) lastSvr
        svrToFftM = countPathsM g 1 svrNode fftNode 0
        -- svrToFft' = ways g svrNode fftNode
        -- fftToDac' = ways g fftNode dacNode
        -- dacToOut' = ways g dacNode outNode
        -- stopNodes = S.insert dacNode (collectNodes dacToOut)
        -- (A dacToFft) = bfsAllPaths g fftNode (\n -> S.member n stopNodes) lastDac
        -- (A dacToFft) = bfsAllPaths g dacNode fftNode (const True)
        -- (A fftToOut) = bfsAllPaths g fftNode outNode (const True)
        -- (A fftToDac) = bfsAllPaths g fftNode dacNode (const True)
        -- (A dacToOut) = bfsAllPaths g dacNode outNode (const True)
    -- putStrLn $ show $ svrToFft
    -- putStrLn $ show $ length svrToFft
    -- putStrLn $ show $ dacToOut
    -- putStrLn $ show $ length dacToOut
    -- putStrLn $ show $ fftToDac
    print ("you out dfs", ways g youNode outNode)
    print ("you out count", countPaths g 1 youNode outNode 0)
    print ("svr fft", svrToFft)
    print ("fft dac", fftToDac)
    print ("dac out", dacToOut)
    print ("svr fft dac out", svrToFft*fftToDac*dacToOut)
    print ("bfs svr fft", bfsSvrToFft)
    -- print ("m svr fft", showIng svrToFftM)
    print ("svr fft'", svrToFft')
    print ("fft dac'", fftToDac')
    print ("dac out'", dacToOut')
    print ("svr fft dac out'", svrToFft'*fftToDac'*dacToOut')
    print ("svr vvw", evalState (dfs g svrNode (stringToNode "sfo")) M.empty)

doDfs g nodeStr = evalState (dfs g (stringToNode "svr") (stringToNode nodeStr)) M.empty

testPart1 :: FilePath -> IO ()
testPart1 fname = do 
    contents <- readFile fname
    let g = buildGraph contents
    print $ countPaths g 1 (stringToNode "you") (stringToNode "out") 0

test :: FilePath -> IO ()
test fname = do 
    contents <- readFile fname
    let g = buildGraph contents
        a = stringToNode "aaa"
        b = stringToNode "bbb"
        e = stringToNode "eee"
        l = stringToNode "lll"
        gg = stringToNode "ggg"
        m = stringToNode "mmm"
        p = stringToNode "ppp"
        s = stringToNode "sss"
        r = stringToNode "rrr"
        h = stringToNode "hhh"
        t = stringToNode "ttt"
        u = stringToNode "uuu"
        y = stringToNode "yyy"

        ing = countPathsM g 1 a m 0
        count = evalState (dfs g h m) M.empty
    print $ map (\(k,v) -> (nodeToString <$> k, v)) $ M.toAscList ing
    print count
