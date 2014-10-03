-- Chapter 8 Stuff, this was mostly inductively proving properties
-- of primitive recursively defined functions. Mostly paperwork

module Tester where

-- Thing from book
fact :: Int -> Int
fact n
  | n==0       = 1
  | otherwise  = n * fact (n-1)

-- A def of mult so that 0 `mult` (fact (-2)) equals zero
mult :: Int -> Int -> Int
mult 0 _ = 0
mult a b = a*b

shunt :: [a] -> [a] -> [a]
shunt [] ys = ys
shunt (x:xs) ys = shunt xs (x:ys)

-- Amazing quick-sort algorithm again (on one line!)
qSort :: Ord a => [a] -> [a] -- 'Ord a' is required because [a] needs to be ordered for qSort to work
qSort [] = [] ; qSort (x:xs) = qSort [y | y<-xs, y<=x] ++ [x] ++ qSort [y | y<-xs, y>x]
