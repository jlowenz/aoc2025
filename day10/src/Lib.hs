module Lib where

import Control.Applicative (liftA3)
import qualified Data.Array as A
import Data.Bits
import Data.Char (digitToInt)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Sort (sortBy, sortOn)
import Debug.Trace 
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, digit)
import Text.Parsec.Combinator (many1)
import Text.Printf 
import Data.List (group)
import GHC.Base (VecElem(Int16ElemRep))

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Buttons = B (A.Array Int Int) deriving Show
data JoltageRequirements = J (A.Array Int Int) deriving Show

data Machine = M {
    targetLightState :: Int,
    buttons :: Buttons,
    buttonsPressed :: Int,
    joltageRequirements :: JoltageRequirements }
    deriving Show

parsePanelL = char '['
parsePanelR = char ']'

parseOn :: Parsec String st Int
parseOn = 1 <$ char '#'

parseOff :: Parsec String st Int
parseOff = 0 <$ char '.'

parseOnOrOff :: Parsec String st Int
parseOnOrOff = parseOn <|> parseOff

parseLightState :: Parsec String st [Int]
parseLightState = many1 parseOnOrOff

toInt :: [Int] -> Int
toInt = go 0 0
    where 
        go :: Int -> Int -> [Int] -> Int 
        go _ acc [] = acc
        go b acc (x:xs) = go (b+1) (acc + (x * 2^b)) xs

parseLightPanel :: Parsec String st Int
parseLightPanel = toInt <$> (parsePanelL *> parseLightState <* parsePanelR)

digitOrComma :: Parsec String st Int
digitOrComma = digitToInt <$> try digit <* optional (char ',')

natural :: Parsec String st Int
natural = read <$> (many1 (try digit))

numOrComma :: Parsec String st Int
numOrComma = natural <* optional (char ',')

buildButton :: [Int] -> Int
buildButton = go 0
    where 
        go :: Int -> [Int] -> Int
        go acc [] = acc
        go acc (x:xs) = go (acc + 2^x) xs

optSpace = many (char ' ')
lParen = optSpace *> char '('
rParen = char ')' <* optSpace
lBrace = optSpace *> char '{'
rBrace = char '}'

parseButton :: Parsec String st Int 
parseButton = buildButton <$> (lParen *> (many1 digitOrComma) <* rParen)

toIntArray :: [Int] -> A.Array Int Int
toIntArray b = A.listArray (0,n-1) b
    where n = length b

toButtons :: [Int] -> Buttons 
toButtons b = B $ toIntArray b

parseButtons :: Parsec String st Buttons
parseButtons = toButtons <$> (many1 parseButton)

toJoltage :: [Int] -> JoltageRequirements
toJoltage b = J $ toIntArray b

parseJoltage :: Parsec String st JoltageRequirements
parseJoltage = toJoltage <$> (lBrace *> (many1 numOrComma) <* rBrace)

buildMachine :: Int -> Buttons -> JoltageRequirements -> Machine
buildMachine panel buttons jolt = M panel buttons 0 jolt

parseMachine :: Parsec String st Machine
parseMachine = liftA3 buildMachine parseLightPanel parseButtons parseJoltage

parseMachines :: [String] -> [Machine]
parseMachines = mapMaybe go
    where
        go :: String -> Maybe Machine 
        go s = case parse parseMachine "" s of
            Left _ -> Nothing 
            Right machine -> Just machine

loadMachines :: String -> [Machine]
loadMachines = parseMachines . lines

searchPath :: Int -> [Int]
searchPath nButtons = fst $ unzip $ sortOn snd searchField
    where
        combs :: [Int]
        combs = [1..(2^(nButtons+1)-1)]
        searchField :: [(Int,Int)]
        searchField = zip combs (map popCount combs)
        numBits :: (Int,Int) -> (Int,Int) -> Ordering
        numBits a b = compare (snd a) (snd b)

type SearchPaths = A.Array Int [Int]
makeSearchPaths :: Int -> SearchPaths
makeSearchPaths n = A.listArray (1,n) $ map searchPath [1..n]

combineButtons :: Buttons -> Int -> Int -> Int
combineButtons _ 0 _ = 0
combineButtons b@(B bts) n k
    | n .&. 1 /= 0 = (bts A.! k) .^. combineButtons b (shiftR n 1) (k+1)
    | otherwise = combineButtons b (shiftR n 1) (k+1)

findButtonPress :: Machine -> [Int] -> Machine
findButtonPress m@(M target btns _ _) paths = m{buttonsPressed = go paths}
    where 
        go :: [Int] -> Int
        go [] = trace ("found no combo: " ++ (printf "%b" target) ++ show m) 0
        go (x:xs) = if combineButtons btns x 0 == target 
            then x 
            else go xs

numPaths  :: Machine -> Int
numPaths (M _ (B btns) _ _) = (snd $ A.bounds btns)

part1fn :: String -> Int
part1fn s = sum $ map go $ loadMachines s
    where 
        sp = makeSearchPaths 12
        go :: Machine -> Int
        go m = popCount $ buttonsPressed $ findButtonPress m paths
            where
                paths = sp A.! (numPaths m)

part1 :: String -> IO ()
part1 fname = do 
    contents <- readFile fname
    putStrLn $ show $ part1fn contents

testBinary :: [String]
testBinary = map (printf "%b") ([0..16]::[Int])

printBinary :: Int -> String
printBinary n = printf "%b" n

-- (Integer) LP problem
-- minimize sum(x_i)
-- s.t.
-- x1 + x3 >= b1
-- x2 + x3 >= b2
-- ...
