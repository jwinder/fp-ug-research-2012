{-
  Chapter 5 :: Tuples and Lists!

  Tuple := A collection (of predetermined size) of items of (not necessarily same) predetermined types.
  List := A collection (of arbitrary/variable size) of items of identical type.
-}

module Tester where

import Prelude hiding (elem)
import Char
import List

-- Example of a list of tuples by a store inventory

-- type Basket = [(String,Integer)]
type ShopItem = (String,Integer)
type Basket = [ShopItem]

-- A function that returns the min/max of two integers in a tuple (min,max)::(Integer,Integer)

miniMax :: Integer -> Integer -> (Integer,Integer)
miniMax a b
  | a<b       = (a,b)
  | otherwise = (b,a)

-- Functions over tuples or "patterns"

addPair :: (Integer,Integer) -> Integer
--addPair (x,y) = x+y
addPair p = fst p + snd p -- fst & snd are built in functions that operate on pairs

shiftRight :: ((Integer,Integer),Integer) -> (Integer,(Integer,Integer))
shiftRight ((a,b),c) = (a,(b,c))

shiftLeft :: (Integer,(Integer,Integer)) -> ((Integer,Integer),Integer)
shiftLeft (a,(b,c)) = ((a,b),c)

name :: ShopItem -> String
name (n,p) = n

price :: ShopItem -> Integer
price (n,p) = p

-- A more efficient definition of a fibonacci calculator than simple recursion

fibStep :: (Integer,Integer) -> (Integer,Integer)
fibStep (u,v) = (v,u+v)

fibPair :: Integer -> (Integer,Integer)
fibPair n
  | n==0       = (0,1)
  | otherwise  = fibStep (fibPair (n-1))

fib :: Integer -> Integer
fib = fst.fibPair

-- More fun with tuples

type Trippio = (Integer,Integer,Integer)

maxThree :: Trippio -> Integer
maxThree (a,b,c)
  | a >= b && a >= c  = a
  | b >= c            = b 
  | otherwise         = c

minThree :: Trippio -> Integer
minThree (a,b,c) 
  | a <= b && a <= c  = a
  | b <= c            = b
  | otherwise         = c

between :: Integer -> Integer -> Integer -> Bool
between x y z = (x <= y && y <= z) || (z <= y && y <= x)

middle :: Trippio -> Integer
middle (a,b,c)
  | between b a c  = a
  | between a b c  = b
  | otherwise      = c 

orderTriple :: Trippio -> Trippio
orderTriple t = (minThree t, middle t, maxThree t)

-- A fun function to specify when a planar line crosses the x-axis (at y=0)
-- Returns (x-intercept,True) when the line crosses the x-axis only once
-- Otherwise returns (0,False)

xIntercept :: (Float,Float) -> (Float,Bool)
xIntercept (m,b) -- m := slope | b := y-intercept
  | m == 0     = (0,False)
  | otherwise  = ((-b)/m,True) 

-- Fun List stuff!

addPairs :: [(Integer,Integer)] -> [Integer]
addPairs pairList = [m+n | (m,n) <- pairList]

addOrdPairs :: [(Integer,Integer)] -> [Integer]
addOrdPairs pairs = [x+y | (x,y) <- pairs, x<y]

digits :: String -> String
digits st = [ch | ch <- st, isDigit ch]

notDigits :: String -> String
notDigits st = [ch | ch <- st, not (isDigit ch)]

allEven ls = (ls == [x | x <- ls, even x])
allOdd ls = (ls == [x | x <- ls, odd x])

doubleAll :: [Integer] -> [Integer]
doubleAll ls = [2*x | x <- ls]

capitalize :: String -> String
capitalize st = [toUpper s | s <- st]

-- A function that lists the divisors of a given integer

divisors :: Integer -> [Integer]
divisors n = [m | m <- [1..abs n], mod n m == 0]

isPrime :: Integer -> Bool
isPrime n = (length.divisors) n == 2

-- A function that picks out the occurences of an integer in a list

matches :: Integer -> [Integer] -> [Integer]
matches n list = [m | m <- list, m == n]

-- A function that returns true when a number is in a given list

elem :: Integer -> [Integer] -> Bool
elem n list = length (matches n list) > 0

-- Fun with polymorphism

id :: a -> a -- a is a Generic type
id x = x

pi_1 :: (a,b) -> a
pi_1 (x,y) = x

pi_2 :: (a,b) -> b
pi_2 (x,y) = y

-- Some fun string functions

-- Three Strings that, when printed, are on separate lines

onThreeLines :: String -> String -> String -> String
onThreeLines a b c = a ++ "\n" ++ b ++ "\n" ++ c

-- A list of strings that, when printed, are on separate lines

separateLines :: [String] -> String
separateLines strs = concat [st++"\n" | st <- strs]

-- A function that copies a string a certain number of times and concatenates them all together

duplicate :: String -> Int -> String
duplicate str n = concat (replicate n str)

-- A function that ouputs a table of fibonnaci numbers

fibTable :: Integer -> String
fibTable n = "\n\tn\tfib n\n" ++ concat ["\n\t" ++ show m ++ "\t" ++ show (fib m) | m <- [1..n]] ++ "\n"
