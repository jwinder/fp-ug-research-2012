{-
  Chapter 7 stuff:
  Defining Functions over Lists

  This chapter is mainly about the "under the hood" of Prelude functions
-}

module Tester where

-- All of these prelude functions are re-written later in this script
import Prelude hiding (and,or,sum,product,(++),concat,elem,reverse,zip,take,drop,zip3)
import Char

-- Example of case statement

digits :: String -> String
digits st = [ch | ch<-st, isDigit ch]

-- Function that returns first digit in a string, returning \0 if none are present
-- Recall that [a,b,c,...] can be written as a:[b,c,...] and (x:_) is therefore a list
-- that begins with a value for x, with a wildcard '_' that matches anything

firstDigit :: String -> Char
firstDigit st =
  case (digits st) of
    []     -> '\0'
    (x:_)  -> x

secondDigit :: String -> Char
secondDigit st =
  if digits st == [] then '\0'
  else
    case (tail (digits st)) of
      []     -> '\0'
      (x:_)  -> x

-- Another way to write the same function, using guards
{-firstDigit st
  | null numbers  = '\0'
  | otherwise     = head numbers
  where
    numbers = digits st
-}

-- Function that return returns the first digit in a list plus one and 0 otherwise

intPlusOne :: String -> Int
intPlusOne st = let digit = firstDigit st in if digit == '\0' then 0 else digitToInt digit + 1

twoPlus :: String -> Int
twoPlus st
  | first == '\0' && second == '\0'  = 0
  | second == '\0'                   = digitToInt first
  | otherwise                        = digitToInt first + digitToInt second
  where
    first = firstDigit st
    second = secondDigit st

-- Primitive recursion over lists! These are all ways to define the prelude functions

-- Sum over a list (this is how sum (_:_) is defined!

sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs

-- Now a product as well :)

product :: [Int] -> Int
product [] = 1
product (y:ys) = y * product ys

-- Conjunction/disjuction over lists

and, or :: [Bool] -> Bool

and [] = True
and (x:xs) = x && and xs

or [] = False
or (y:ys) = y || or ys

-- How about the (++) function, which concatentates two lists

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x:(xs++ys)

-- Concatenate a list of lists into a single list.
-- In other words: concat [e1,e2,...] = e1 ++ e2 ++ ...

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs

-- Elem returns true if an integer is an element of a list

elem :: Int -> [Int] -> Bool
elem x [] = False
elem x (y:ys) = x==y || elem x ys

-- Double all elements in a list, the first way using a list comprehension,
-- the second way uses primitive recursion

doubleAll :: [Int] -> [Int]
--doubleAll xs = [2*x | x<-xs]
doubleAll [] = []
doubleAll (x:xs) = 2*x : doubleAll xs

-- Selecting all even elements of a list

selectEven :: [Int] -> [Int]
--selectEven xs = [x | x<-xs, even x]
selectEven [] = []
selectEven (x:xs)
  | even x     = x : selectEven xs
  | otherwise  = selectEven xs

-- Insertion sort, OMG!

iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins x (y:ys)
  | x <= y     = x:(y:ys)
  | otherwise  = y : ins x ys

-- Number of times an element occurs in a list

elemNum :: Int -> [Int] -> Int
elemNum x [] = 0
elemNum x (y:ys) = (if x==y then 1 else 0) + elemNum x ys
--elemNum x (y:ys)
--  | x==y       = 1 + elemNum x ys
--  | otherwise  = elemNum x ys
--elemNum x xs = length [y | y<-xs, y==x] -- Using list comprehensions

unique :: [Int] -> [Int]
unique [] = []
unique (x:xs)
  | elem x xs  = unique [y | y<-xs, y/=x]
  | otherwise  = x : unique xs
--unique xs = [x | x<-xs, elemNum x xs == 1] -- List comprehension solution

reverse :: [Int] -> [Int]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

-- General recursion!

-- Normal prelude zip function

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys) = (x,y) : zip xs ys
zip (x:xs) [] = []
zip [] zs = []

-- Normal prelude take function

take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x:xs)
  | n>0        = x : take (n-1) xs
  | otherwise  = error "PreludeList.take: negative argument"

-- Quick sort algorithm

qSort :: [Int] -> [Int]
qSort [] = []
qSort (x:xs) = qSort [y | y<-xs, y<=x] ++ [x] ++ qSort [y | y<-xs, y>x]

-- Drop!

drop :: Int -> [a] -> [a]
drop 0 (x:xs) = (x:xs)
drop _ [] = []
drop n (x:xs)
  | n>0        = drop (n-1) xs
  | otherwise  = error "PreludeList.drop: negative argument"

-- Zip3! Three lists, not just two

zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3 (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3 xs ys zs
zip3 [] _ _ = []
zip3 _ [] _ = []
zip3 _ _ [] = []

--zip3 x y z = [(a,b,c) | (a,(b,c))<-zip x (zip y z)] -- Another way, using zip and list comprehensions
