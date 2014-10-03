module Tester where

import Prelude hiding (max,min)
import Char hiding (toUpper,isDigit)

-- One way to define exclusive or

--xor :: Bool -> Bool -> Bool
--xor x y = (x || y) && not (x && y)

-- A second way to define exclusive or

xor :: Bool -> Bool -> Bool
xor x y = x /= y

-- A third way to define exclusive or

--xor :: Bool -> Bool -> Bool
--xor True x = not x
--xor False x = x

-- Negated exclusive or

--nxor :: Bool -> Bool -> Bool
--nxor x y = not (xor x y)

-- Second negated exclusive or

nxor :: Bool -> Bool -> Bool
nxor x y = x == y

-- Boring not definition

mynot :: Bool -> Bool
mynot True = False
mynot False = True

-- Negated And definition

--nAnd :: Bool -> Bool -> Bool
--nAnd x y = not (x && y)

-- A second negated And definition

nAnd :: Bool -> Bool -> Bool
nAnd True True = False
nAnd x y = True

-- Function that tests equality of three numbers

threeEqual :: Int -> Int -> Int -> Bool
threeEqual a b c = (a==b) && (b==c)

-- Function that checks if three numbers are distinct

threeDiff :: Int -> Int -> Int -> Bool
threeDiff a b c = (a/=b) && (a/=c) && (b/=c)

-- Function that returns true if four numbers given are equal

fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual a b c d = (threeEqual a b c) && (threeEqual b c d)

-- Testing some overloading to see how it works

equal :: Int -> Int -> Bool
equal = (==)

-- First guard example
-- Guard := 'piecewise' boolean expression of sorts
-- The first first guard is evaluated first, if it is true then the function is equal to the first,
-- if it is false, the second guard is evaluated, and so on...
-- the 'otherwise' guard is always true
{-
max :: Int -> Int -> Int
max x y
   | x >= y     = x   -- max x y equals x if the guard is true
   | otherwise  = y   -- if the guard is false, then max x y equals y
-}
min :: Int -> Int -> Int
min x y
   | x <= y     = x
   | otherwise  = y

{-
maxThree :: Int -> Int -> Int -> Int
maxThree x y z
   | x >= y && x >= z  = x
   | y >= z            = y
   | otherwise         = z
-}

maxThree :: Int -> Int -> Int -> Int
maxThree x y z = max (max x y) z

{-
factorial :: Integer -> Integer
factorial n
   | n <= 1 = 1
   | otherwise = n*factorial(n-1)
-}

-- If-then-else construct example

max :: Int -> Int -> Int
max x y = if x >= y then x else y

factorial :: Integer -> Integer
factorial n = if n <= 1 then 1 else n * factorial (n-1)

-- Character stuff
-- Some nice functions:
--   ord :: Char -> Int -- Converts a char to its ascii numeric value
--   chr :: Int -> Char -- Converts an ascii value to its char equivalent

offset :: Int
offset = ord 'A' - ord 'a'

toUpper :: Char -> Char
toUpper ch
   | 'a' <= ch && ch <= 'z'  = chr (ord ch + offset)
   | otherwise               = ch

toLower :: Char -> Char
toLower ch
   | 'A' <= ch && ch <= 'Z'  = chr (ord ch - offset)
   | otherwise               = ch

isDigit :: Char -> Bool
isDigit ch = ('0' <= ch) && (ch <= '9') -- the comparisons refer to the natural ascii integer value ordering

offerSet :: Int
offerSet = 48

charToNum :: Char -> Int
charToNum ch
   | '0' <= ch && ch <= '9'  = ord ch - offerSet
   | otherwise               = -1

-- Some float stuff

averageThree :: Int -> Int -> Int -> Float
averageThree a b c = fromIntegral (a+b+c) / 3.0

-- THIS FUNCTION IS NOT CORRECT! I'm just lazy at the moment
howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage a b c
   | threeEqual a b c = 0
   | otherwise        = 1

-- infix integer min function
(&&&) :: Int -> Int -> Int
(&&&) = min

-- infix max function
(%%%) :: Int -> Int -> Int
(%%%) = max
