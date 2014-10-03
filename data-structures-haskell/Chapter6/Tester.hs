{-
  Chapter 6!
  Programming with lists
-}

module Tester where

import Char

-- Local definitions!

sumSquares m n = sqM + sqN
  where
    sqM = m*m
    sqN = n*n

addPairwise :: [Integer] -> [Integer] -> [Integer]

-- This function drops extra information if one list is longer than the other

addPairwise list1 list2 = [m+n | (m,n) <- zip list1 list2]

-- What about if we want addPairwise [1,7] [8,4,2,67] --> [9,11,2,67] ??
{-
addPairwise' list1 list2 = front ++ rear
  where
    minLength = min (length list1) (length list2)
    front     = addPairwise (take minLength list1) (take minLength list2)
    rear      = drop minLength list1 ++ drop minLength list2
-}

addPairwise' list1 list2 = front ++ rear
  where
    minLength      = min (length list1) (length list2)
    front          = addPairwise front1 front2
    rear           = rear1 ++ rear2
    (front1,rear1) = splitAt minLength list1
    (front2,rear2) = splitAt minLength list2

{-
  PROPER BLOCKING SYNTAX:

f p1 p2 ... pk
  | g1        = e1
  | g2        = e2
  | ...
  | otherwise = er
  where
    v1  a1 ... an = r1
    v2            = r2
    ...
-}

-- Local definitions to expressions alone, the values x and y are only
-- assigned in the expressions. These need to be on a single line always

--let x = 3+2 in x^2 + 2*x - 4
--let x = 3+2; y = 5-1 in x^2 + 2*x - y

-- Some scope practice

isOdd, isEven :: Integer -> Bool

isOdd n
  | n<=0       = False
  | otherwise  = isEven (n-1)

isEven n
  | n<0        = False
  | n==0       = True
  | otherwise  = isOdd (n-1)

-- It is possible to use variables/definitions in the guard before they are defined the where clause

maxsq x y
  | sqx > sqy  = sqx
  | otherwise  = sqy
  where
    sqx = sq x
    sqy = sq y
    sq :: Integer -> Integer
    sq z = z*z

-- It is possible to use two definitions or variables with the same name, like below
-- Although this practice can easily be avoided

maxsqr x y
  | sq x > sq y  = sq x
  | otherwise    = sq y
  where
    sq x = x*x

-- A function to return the maximum number in a collection of three as well as the number of
-- times it occurs in the collection

--matches n list = [m | m <- list, m == n]

-- Not allowed to use where-definitions here
--maxThreeOccurs a b c = (max a (max b c),length (matches (max a (max b c)) [a,b,c]))

maxThreeOccurs a b c = (biggest,times)
  where
    biggest = max a (max b c)
    times   = length [n | n <- [a,b,c], n == biggest]
