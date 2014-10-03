module Tester where

import Prelude hiding (map,filter,zipWith,length,foldr1,foldr,concat,and,or)
import Char

-- General map function

--map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x<-xs]
--map f [] = []
--map f (x:xs) = f x : map f xs

double x = 2*x
doubleAll xs = map double xs

convertChars :: [Char] -> [Int]
convertChars xs = map ord xs

-- General filter function

--filter p xs = [x | x<-xs, p x]
filter p [] = []
filter p (x:xs)
  | p x        = x : filter p xs
  | otherwise  = filter p xs

digits xs = filter isDigit xs

eveny xs = filter even xs

qSort [] = []
qSort (x:xs) = qSort [y | y<-xs, y<=x] ++ [x] ++ qSort [y | y<-xs, y>x]
isSorted xs = (xs == qSort xs)

-- This one takes in a list of lists and returns the lists that are sorted
sorted xs = filter isSorted xs

add x y = x+y

-- zipWith takes each corresponding element of a list and performs f to them
-- for example zipWith add [1] [2] = [3]
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith f _ _ = []

length = sum . map (const 1)
--length xs = sum (map (const 1) xs)

-- Squares fun
sqr n = n*n
squares ns = map sqr ns

sumSqrs = sum . squares

-- Positives fun
isPos n = n>0
positives ns = filter isPos ns

-- Primitive recursion and folding

--foldr1 f [x] = x
--foldr1 f (x:xs) = f x (foldr1 f xs)

-- regular foldr returns s in the event of an empty list
-- foldr == fold to the "right", s is a default value
foldr f s [] = s
foldr f s (x:xs) = f x (foldr f s xs)

-- Thus foldr1 means fold right with the requirement
-- that the list has at least one element
foldr1 f (x:xs) = foldr f x xs

concat :: [[a]] -> [a]
concat xs = foldr (++) [] xs

and :: [Bool] -> Bool
and bs = foldr (&&) True bs

or :: [Bool] -> Bool
or bs = foldr (||) False bs

-- Reverse a list
rev :: [a] -> [a]
rev xs = foldr snoc [] xs

-- (snoc == cons in reverse)
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

-- Sum of squares from 1 .. n, again
sumOfSquares n = foldr (+) 0 (map sqr [1..n])

-- Sum of squares of a list of integers
sumOfInts ns = foldr (+) 0 (map sqr ns) 
