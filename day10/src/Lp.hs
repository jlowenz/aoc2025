module Lp where 

import Prelude hiding (Num(..))

import Algebra.Classes
import Control.Monad (mapM, forM_)
import Control.Monad.LPMonad
import qualified Data.Array as A
import Data.Bits 
import Data.LinearProgram.Common
import Data.LinearProgram
import Data.LinearProgram.GLPK
import qualified Data.Map as M
import Data.LinearProgram.LinExpr
import Data.Maybe (mapMaybe, catMaybes)


import Lib 

objFun :: LinFunc String Int
objFun = linCombination $ map (1 *&) ["x1", "x2", "x3", "x4", "x5", "x6"]

n *& v = (n,v)

makeVar :: Int -> String
makeVar i = "x" ++ (show i)

makeObjective :: Buttons -> LinFunc String Int
makeObjective (B btns) = linCombination $ map (1 *&) vars 
    where
        vars = map makeVar [0..(snd $ A.bounds btns)]


type ConstraintEqVal = (LinFunc String Int, Int)


makeConstraintComb :: Int -> Int -> Buttons -> ConstraintEqVal
makeConstraintComb bit target (B btns) = (linCombination $ map (1 *&) vars, target)
    where
        (minB, maxB) = A.bounds btns
        indices = [minB..maxB]
        toVar :: Int -> Maybe String
        toVar i = if testBit (btns A.! i) bit then Just $ makeVar i else Nothing
        vars = mapMaybe toVar indices


makeConstraintCombs :: Buttons -> JoltageRequirements -> [ConstraintEqVal]
makeConstraintCombs bs (J jreqs) = foldr mkComb [] (A.assocs jreqs)
    where
        mkComb :: (Int,Int) -> [ConstraintEqVal] -> [ConstraintEqVal]
        mkComb (i,t) combs = makeConstraintComb i t bs : combs

makeLP :: Buttons -> JoltageRequirements -> LP String Int
makeLP b@(B btns) j@(J jreqs) = execLPM $ do
    setDirection Min
    setObjective $ makeObjective b
    forM_ (makeConstraintCombs b j) (\(comb, t) -> equalTo comb t)
    let (minB, maxB) = A.bounds btns
        vars = map makeVar [minB..maxB]
    forM_ vars (\v -> varGeq v 0)
    forM_ vars (\v -> setVarKind v IntVar)


minButtonPresses :: Machine -> IO (Maybe Int)
minButtonPresses (M _ btns _ jreqs) = do 
    let lp = makeLP btns jreqs
    (ret, Just (obj, sol)) <- glpSolveVars mipDefaults lp
    if ret == Success then return $ Just (round obj) else return Nothing 


part2fn :: String -> IO Int
part2fn s = do 
    let machines = loadMachines s
    btnPresses <- mapM minButtonPresses machines
    putStrLn $ show btnPresses
    return $ Prelude.sum $ catMaybes btnPresses

part2 :: String -> IO ()
part2 fname = do
    contents <- readFile fname
    answer <- part2fn contents
    putStrLn $ show $ answer

-- Ax >= b
-- 4x6
-- 0 0 0 0 1 1
-- 0 1 0 0 0 1
-- 0 0 1 1 1 0
-- 1 1 0 1 0 0

-- [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
-- [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
-- [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}

lp :: LP String Int
lp = execLPM $ do
  setDirection Min
  setObjective objFun
  geqTo (linCombination $ map (1 *&) ["x5","x6"]) 3
  geqTo (linCombination $ map (1 *&) ["x2","x6"]) 5
  geqTo (linCombination $ map (1 *&) ["x3","x4","x5"]) 4
  geqTo (linCombination $ map (1 *&) ["x1","x2","x4"]) 7
  varGeq "x1" 0
  varGeq "x2" 0
  varGeq "x3" 0
  varGeq "x4" 0
  varGeq "x5" 0
  varGeq "x6" 0
  setVarKind "x1" IntVar
  setVarKind "x2" IntVar
  setVarKind "x3" IntVar
  setVarKind "x4" IntVar
  setVarKind "x5" IntVar
  setVarKind "x6" IntVar

solveLP = print =<< glpSolveVars mipDefaults lp

-- [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
-- 1 0 1 1 0  7
-- 0 0 0 1 1  5
-- 1 1 0 1 1  12
-- 1 1 0 0 1  7
-- 1 0 1 0 1  2
objFun2 :: LinFunc String Int
objFun2 = linCombination $ map (1 *&) ["x1", "x2", "x3", "x4", "x5"]


lp2 :: LP String Int
lp2 = execLPM $ do
  setDirection Min
  setObjective objFun2
  geqTo (linCombination $ map (1 *&) ["x1","x4", "x5"]) 7
  geqTo (linCombination $ map (1 *&) ["x4","x5"]) 5
  geqTo (linCombination $ map (1 *&) ["x1","x2","x4","x5"]) 12
  geqTo (linCombination $ map (1 *&) ["x1","x2","x5"]) 7
  geqTo (linCombination $ map (1 *&) ["x1","x3","x5"]) 2
  varGeq "x1" 0
  varGeq "x2" 0
  varGeq "x3" 0
  varGeq "x4" 0
  varGeq "x5" 0
  varGeq "x6" 0
  setVarKind "x1" IntVar
  setVarKind "x2" IntVar
  setVarKind "x3" IntVar
  setVarKind "x4" IntVar
  setVarKind "x5" IntVar
  setVarKind "x6" IntVar

solveLP2 = print =<< glpSolveVars mipDefaults lp2

-- [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
-- 1 1 1 0
-- 1 0 1 1
-- 1 0 1 1
-- 1 1 0 0
-- 1 1 1 0
-- 0 0 1 0

objFun3 :: LinFunc String Int
objFun3 = linCombination $ map (1 *&) ["x1", "x2", "x3", "x4"]


lp3 :: LP String Int
lp3 = execLPM $ do
  setDirection Min
  setObjective objFun3
  geqTo (linCombination $ map (1 *&) ["x1","x2", "x3"]) 10
  geqTo (linCombination $ map (1 *&) ["x1","x3","x4"]) 11
  geqTo (linCombination $ map (1 *&) ["x1","x3","x4"]) 11
  geqTo (linCombination $ map (1 *&) ["x1","x2"]) 5
  geqTo (linCombination $ map (1 *&) ["x1","x2","x3"]) 10
  geqTo (linCombination $ map (1 *&) ["x3"]) 5
  varGeq "x1" 0
  varGeq "x2" 0
  varGeq "x3" 0
  varGeq "x4" 0
  setVarKind "x1" IntVar
  setVarKind "x2" IntVar
  setVarKind "x3" IntVar
  setVarKind "x4" IntVar

solveLP3 = glpSolveVars mipDefaults lp3