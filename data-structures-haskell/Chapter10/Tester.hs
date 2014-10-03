module Tester where

import Prelude hiding ((.),succ,flip)
import Char

--double x = 2*x
double = (2*) -- Partial application (used later in this script moreso)

--succ n = n+1
succ = (1+)

--iter n f
--  | n>0  = f . iter (n-1) f
--  | otherwise = id
iter n f = foldr (.) id (replicate n f)

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x = f (g x)

-- Since (g.f) x = g(f x), or that g is applied to the result of f x, it is easy
-- to define a reverse order composition, (f .> g) x means apply f to x, then apply
-- g to the result of f x.
--(.>) :: (a -> b) -> (b -> c) -> (a -> c)
f .> g = g . f

-- Function that takes a list of functions as an argument and composes them all
--composey [] = id 
--composey (f:fs) = (f . composey fs)
composey fs = foldr (.) id fs

twice f = (f.f)

-- Function that returns a function as a result
-- For example addNum takes in n and returns a function that takes in m and return the sum n+m
-- (addNum 3) 2 ==> addN 2 ==> 3+2 = 5
--addNum :: Int -> (Int -> Int)
addNum n = addN
  where
    addN m = n+m

-- A way to do this same function using lambda functions
--addyNum :: Int -> Int -> Int
addyNum n = (\m -> n+m)

sq n = n*n
add a b = a+b
comp2 :: (a -> b) -> (b -> b -> c) -> (a -> a -> c)
comp2 f g = (\x y -> g (f x) (f y)) -- A lambda thing

-- The gist of lambda functions is that:
--   f x y z = result
--   \x y z -> result
-- have the same result

-- Thing, i.e. iterSuck 5 1 = 1+1+1+1+1+1 = 6
iterSuck = (\n -> iter n succ)
iterDub = (\n -> iter n double)


-- Thing
eff x y = x - y 
revEff = (\x y -> eff y x) -- lambda expression to reverse arguments

-- Funcion to reverse arguments and return function
flip :: (a -> b -> c) -> (b -> a -> c)
flip f = (\x y -> f y x)

-- Function such that total f = f 0 + f 1 + ... + f n
total :: (Int -> Int) -> (Int -> Int)
total f = \n -> foldr (+) 0 (map f [0..n])

-- Derivative approximation
slope :: (Float -> Float) -> (Float -> Float)
slope f = \x -> (f x - f (x - 0.00001)) / (x - (x - 0.00001))

-- Partial function application, in other words, only supplying *some* of the arguments...

-- multiply 2 3 = 6 but multiply 5 = 5*y, so this is being partially applied to one of its arguments
multiply :: Int -> Int -> Int
multiply x y = x*y

-- This works great because of partial application! multiply 2 is being partially applied, and map (multiply 2)
-- is being partially applied since it is not given its second argument, either.
-- The second argument of multiply 2 is being passed to map in order to form the doubleAll function
-- There are distinct advantages to this rather than doubleAll xs = map (multiply 2) xs
-- Notice that :type multiply, :type multiply 2, :type multiply 2 3, :type doubleAll, :type doubleAll [1]
-- all have different types!
doubleAll :: [Int] -> [Int]
doubleAll = map (multiply 2)

-- Fun fact!
-- Every function in haskell takes in one argument! If the application yields a function, then this function
-- may be applied to a further argument, and so on. Thus multi-argument functions are applied partially, always.
-- So, multiply:: Int -> Int -> Int, is actually multiply:: Int -> (Int -> Int),
-- and multiply 2 :: Int -> Int, and (multiply 2) 5 :: Int, which is the same function as multiply 2 5 :: Int,
-- The function space symbol (->) is said to be right-associative, and function application is said to be
-- left-assocative

-- In general, 'op' will put its argument to the side which completes its application, that is
-- (op x) y = y op x
-- (x op) y = x op y

-- A cool function that adds one to ech element of a list, then filters out negatives
--funStuff = filter (>0) . map (+1)
funStuff = map (+1) .> filter (>0)

-- Filter out events
getEvens :: [Int] -> [Int]
getEvens = filter ((`mod` 2) .> (==0))

--Currying and uncurrying!
mult :: Int -> Int -> Int
mult x y = x*y -- Takes arguments one at a time

multUC :: (Int,Int) -> Int
multUC (x,y) = x*y -- uncurried gives arguments bundled together

-- Curried is prefered, and curried versions allow for partial application
-- notice the curry and uncurry functions:
-- curry multUC = mult
-- uncurry mult = multUC
