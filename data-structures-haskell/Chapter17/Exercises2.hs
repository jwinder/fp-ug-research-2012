module Exercises2 where
import List

--------
-- 17.22
--------

--Define the infinite lists of factorial and Fibonacci numbers

facs :: [Integer]
facs = map fac [0..]

fac n = if n<=1 then 1 else n* fac (n-1)

-- Another solution to factorials

factorials = 1 : zipWith (*) [1..] factorials

fibs :: [Integer]
fibs = 0:1:[x+y | (x,y)<- zip fibs (tail fibs)]

--------
-- 17.23
--------

-- Define the function

factors :: Integer -> [Integer]

-- which returns a list of the factors of a positive integer.

factors n = [m | m<-[1..n], mod n m == 0]

-- And the hamming numbers, i.e. numbers whose only prime factors are 2,3 and 5
-- hamming = [1,2,3,4,5,6,8,9,10,12,15,...]

hamming = [n | n<-[1..], intersect (factors n) (drop 3 (takeWhile (<=n) primes)) == []]

primes = sieve [2 ..]
sieve (x:xs) = x : sieve [y | y<-xs, mod y x > 0]

--------
-- 17.24
--------

-- Define the function

runningSums :: [Integer] -> [Integer]

-- which calculates the running sums: [0, a0, a0+a1, a0+a1+a2] of the list [a0, a1, a2, ...]

runningSums [] = []
runningSums (x:[]) = x:[]
runningSums (x:y:zs) = x: runningSums (x+y:zs)

--------
-- 17.25
--------

-- Define the function

prod :: [a] -> [b] -> [(a,b)]

-- and use it to define a pythagTriples function.

prod xs ys = [(x,y) | x<-xs, y<-ys]

--------
-- 17.26
--------

-- Give a definition of the list [2^n | n<-[0..]] using a process network based
-- on scanl1'.

powersTwo = scanl (*) 2 [2,2..]

--------
-- 17.27
--------

-- How would you select certain elements of an infinite list? For instance,
-- how would you keep running sums of the positive numbers in a list of numbers?

-- My first instinct would be to use the prelude Filter function, for example:

-- sumPositives = scanl (+) 0 (filter (>0) numbers)

-- The above approach works well because the entire list "filter (>0) numbers"
-- is not completely evaluated before scanl is started to be evaluated, due to
-- lazy evaluation.

-- If I were given more information, I might refine this approach. For example,
-- if the list were an always-decreasing list of numbers, I would use this:

-- sumPositives = scanl (+) 0 (takeWhile (>0) numbers)

-- Although this wouldn't be an infinite list anymore.

--------
-- 17.28
--------

-- Define a function to merge to inifinite lists. How would you remove duplicate
-- elements?

-- To remove duplicates, the List.nub function comes in handy.

merge xs ys = nub [z | z<-xs, z<-ys]

-- This function is a bit slow, I'll have to keep thinking on this one.
