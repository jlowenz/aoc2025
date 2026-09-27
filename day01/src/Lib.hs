module Lib
    ( readRotations, turnDial, turn, turn0x434, turnFull, Dial (Dial)
    ) where

import Data.Maybe (mapMaybe)
import Text.Parsec
    ( char, digit, (<|>), many1, parse, try, Parsec )


type Filename = String

data Rotation = LRot Integer | RRot Integer
    deriving (Show, Eq, Ord)
data Dial = Dial Integer
    deriving (Show, Eq, Ord)

parseL :: Parsec String st Char
parseL = char 'L'
parseR :: Parsec String st Char
parseR = char 'R'

parseNatural :: Parsec String st Integer
parseNatural = read <$> many1 digit

parseLeft :: Parsec String st Rotation
parseLeft = LRot <$> (parseL *> parseNatural)

parseRight :: Parsec String st Rotation
parseRight = RRot <$> (parseR *> parseNatural)

parseRotation :: Parsec String st Rotation
parseRotation = try parseLeft <|> parseRight

parseRotations :: [String] -> [Rotation]
parseRotations = mapMaybe go 
    where
        go :: String -> Maybe Rotation
        go s = case parse parseRotation "" s of
            Left _ -> Nothing
            Right rot -> Just rot

readRotations :: Filename -> IO [Rotation]
readRotations fname = do
    contents <- readFile fname
    return $ parseRotations $ lines contents

dialCount :: Integer
dialCount = 100

turn :: Integer -> Integer -> (Dial, Integer)
turn d r = (Dial newD, count)
    where 
        newD = mod (d+r) dialCount
        count = if newD == 0 then 1 else 0

turn0x434 :: Integer -> Integer -> (Dial, Integer)
turn0x434 = go 0
    where
        incr 0 = 1
        incr _ = 0
        -- go count d 0 = (Dial d, count)
        go :: Integer -> Integer -> Integer -> (Dial, Integer)
        go count d r 
            | r < 0 = let newD = mod (d-1) dialCount in
                            go (count + incr newD) newD (r+1)
            | r > 0 = let newD = mod (d+1) dialCount in
                            go (count + incr newD) newD (r-1)
            | otherwise = (Dial d, count)

computeCount :: Integer -> Integer -> Integer
computeCount 0 amt = div (abs amt) dialCount
computeCount currD amt 
    | amt < 0 = if newAmt > 0 then 0 else 1 + div (abs newAmt) dialCount
    | otherwise = div (currD + amt) dialCount
        where newAmt = amt + currD

turnFull :: Integer -> Integer -> (Dial, Integer)
turnFull currD amt = (Dial newD, computeCount currD amt)
    where
        newD = mod (currD + amt) dialCount

-- Accept a function that computes the correct 0 counts given a list of rotations
turnDial :: Dial -> [Rotation] -> (Integer -> Integer -> (Dial, Integer)) -> Integer
turnDial dial rots turnFn = go dial rots 0
    where
        go (Dial _) [] count = count
        go (Dial d) ((LRot i):rs) count = let (newD, c) = turnFn d (-i) in
            go newD rs (count + c)
        go (Dial d) ((RRot i):rs) count = let (newD, c) = turnFn d i in
            go newD rs (count + c)

