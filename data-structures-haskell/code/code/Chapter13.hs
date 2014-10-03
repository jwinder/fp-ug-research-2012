
-- 	Haskell: The Craft of Functional Programming
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1999.

-- 	Chapter 13

module Chapter13 where

{- helium changes
replace the range ['a' .. y] by the 2 element list ['a',y]
give versions of member and merge on lists which take explicit
  equality and ordering (functional) parameters
-}

-- Checking types
-- ^^^^^^^^^^^^^^

-- Non-type-correct definitions are included as comments.

example1 = ord 'c' + 3

-- 	example2 = ord 'c' + False

-- 	f n     = 37+n
-- 	f True  = 34

-- 	g 0 = 37
-- 	g n = True

-- 	h x 
-- 	  | x>0         = True
-- 	  | otherwise   = 37

-- 	k x = 34
-- 	k 0 = 35


-- Polymorphic type checking
-- ^^^^^^^^^^^^^^^^^^^^^^^^^

-- Examples without their types; use Hugs to find them out.

f (x,y) = (x , ['a' , y])

g (m,zs) = m + length zs

h = g . f

expr :: Int
expr = length ([]++[True]) + length ([]++[2,3,4]) 

-- The funny function does not type check.

-- 	funny xs = length (xs++[True]) + length (xs++[2,3,4])


-- Type checking and classes
-- ^^^^^^^^^^^^^^^^^^^^^^^^^

-- Membership on lists

-- member :: Eq a => [a] -> a -> Bool

-- member []     y = False
-- member (x:xs) y = (x==y) || member xs y

memberList :: (a -> a -> Bool) -> [a] -> a -> Bool

memberList eq []     y = False
memberList eq (x:xs) y = (eq x y) || memberList eq xs y

-- Merging ordered lists.

merge ord (x:xs) (y:ys) 
  = case ord x y of
  LT ->  x : merge ord xs (y:ys)
  EQ ->  x : merge ord xs ys
  _  ->  y : merge ord (x:xs) ys
merge ord (x:xs) []    = (x:xs)
merge ord []    (y:ys) = (y:ys)
merge ord []    []     = []
