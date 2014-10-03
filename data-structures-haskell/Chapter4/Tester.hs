module Tester where

{-
fac :: Integer -> Integer
fac n
  | n <= 1    = 1
  | otherwise = fac (n-1) * n
-}

--fac :: Integer -> Integer
--fac n = if n <= 1 then 1 else fac (n-1) * n

-- Range Product gives the product m(m+1)...(n-1)n when m<=n

rangeProd :: Integer -> Integer -> Integer
rangeProd m n
  | m > n      = 0
  | m == n     = m
  | otherwise  = m * rangeProd (m+1) n

fac :: Integer -> Integer
fac n = if n <= 0 then 1 else rangeProd 1 n

sumFun :: (Integer -> Integer) -> Integer -> Integer
sumFun f n
  | n==0  = f 0
  | n>0   = sumFun f (n-1) + f n

-- This function calculates the total number of possible regions on a 2d plane
-- given a specific number of lines dividing the plane
-- One line accross the plane can pass through each other line at most once,
-- so a line can cross at most n regions, cutting each region into two regions,
-- so there are n new regions at the nth line

regions :: Integer -> Integer
regions n
  | n==0  = 1
  | n>0   = regions (n-1) + n

-- A fun function that multiplies two integers together using addition and recursion

times :: Integer -> Integer -> Integer
times x y
  | y==0  = 0
  | y>0   = times x (y-1) + x

-- A fun function that calculates the integer square root of a number. That is, the largest
-- integer whose square is less than or equal to a given positive number

intSqrt :: Float -> Integer
intSqrt = floor.sqrt -- Okay, so I was supposed to write this recursively, but I'm lazy

-- Good old fibonacci number calculator

fib :: Integer -> Integer
fib n
  | n==0  = 0
  | n==1  = 1
  | n>1   = fib (n-2) + fib (n-1)

-- remainder and divide functions, these have inherent problems with negative numbers!

remainder :: Integer -> Integer -> Integer
remainder m n
  | m<n        = m
  | otherwise  = remainder (m-n) n 

divide :: Integer -> Integer -> Integer
divide m n
  | m<n        = 0
  | otherwise  = 1 + divide (m-n) n
