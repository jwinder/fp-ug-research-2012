
-- 	Haskell: The Craft of Functional Programming
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1999.

-- 	Chapter 19

-- Time and space behaviour
-- ^^^^^^^^^^^^^^^^^^^^^^^^

module Chapter19 where

{- helium changes
make Eq a => ... into functions over Int
Comment out import and give new definition of map'
change the syntax of operator sections
-}

-- import Prelude hiding (map)

-- Various functions whose complexity is discussed.
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Naive Fibonacci function

fib 0 = 0
fib 1 = 1
fib m = fib (m-2) + fib (m-1)

-- Naive factorial function

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

-- Insertion sort

iSort :: [Int] -> [Int]

iSort []     = []
iSort (x:xs) = ins x (iSort xs)

ins :: Int -> [Int] -> [Int]

ins x [] = [x]
ins x (y:ys) 
  | (x<=y)      = x:y:ys
  | otherwise   = y:ins x ys

-- Quicksort

qSort :: [Int] -> [Int]

qSort []     = []
qSort (x:xs) = qSort [z|z<-xs,z<=x] ++ [x] ++ qSort [z|z<-xs,z>x]

-- Two reverse functions

rev1 []     = []
rev1 (x:xs) = rev1 xs ++ [x]

rev2            = shunt []
shunt xs []     = xs
shunt xs (y:ys) = shunt (y:xs) ys

-- Two multiplication functions

mult n 0 = 0
mult n m = mult n (m-1) + n

russ n 0 = 0
russ n m 
  | (m `mod` 2 == 0)    = russ (n+n) (m `div` 2)
  | otherwise           = russ (n+n) (m `div` 2) + n

-- The merge sort function 

mSort xs 
  | (len < 2)   = xs
  | otherwise   = mer (mSort (take m xs)) (mSort (drop m xs))
    where
    len = length xs
    m   = len `div` 2

mer (x:xs) (y:ys) 
  | (x<=y)      = x : mer xs (y:ys)
  | otherwise   = y : mer (x:xs) ys
mer (x:xs) []   = (x:xs)
mer []     ys   = ys

-- Implementations of sets
-- ^^^^^^^^^^^^^^^^^^^^^^^

-- Sets implemented as _unordered_ lists.

-- type Set a = [a]

-- empty        = []
-- memSet       = member
-- inter xs ys  = filter (member xs) ys
-- union        = (++)
-- subSet xs ys = and (map (member ys) xs)
-- eqSet xs ys  = subSet xs ys && subSet ys xs
-- makeSet      = id
-- mapSet       = map
--  


-- Space behaviour
-- ^^^^^^^^^^^^^^^

-- Lazy evaluation
-- ^^^^^^^^^^^^^^^

-- List examples

exam1 n = [1 .. n] ++ [1 .. n]

exam2 n = list ++ list 
          where 
          list=[1 .. n]

exam3 n = [1 .. n] ++ [last [1 .. n]]

exam4 n = list ++ [last list]
          where
          list=[1 .. n]


-- Saving space?
-- ^^^^^^^^^^^^^

-- A new version of factorial

newFac :: Int -> Int
newFac n = aFac n 1

aFac 0 p = p
aFac n p = aFac (n-1) (p*n)

-- This can be modified thus:
-- 	aFac n p
-- 	  | p==p        = aFac (n-1) (p*n)

-- Miscellaneous functions

sumSquares :: Int -> Int
sumSquares n = sumList (map sq [1 .. n])

sumList = foldr (+) 0
sq n    = n*n



-- Folding revisited
-- ^^^^^^^^^^^^^^^^^

-- Map defined using foldr

map' f = foldr ((:).f) []

-- Factorial using foldr

facFold n = foldr (*) 1 [1 .. n]

-- Examples

foldEx1 n = foldr (&&) True (map ((==)2) [2 .. n])



-- Avoiding re-computation: memoization
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- The Fibonacci numbers

-- A naive algorithm is given earlier in this script.

-- An algorithm which returns a pair of consecutive Fibonacci numbers.

fibP :: Int -> (Int,Int)

fibP 0 = (0,1)
fibP n = (y,x+y)
         where
         (x,y) = fibP (n-1)

-- The list of Fibonacci values, defined directly.

fibs ::[Int]

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)


-- Dynamic programming: maximal common subsequence
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- The naive algorithm ...

mLen :: [Int] -> [Int] -> Int

mLen xs []        = 0
mLen [] ys        = 0
mLen (x:xs) (y:ys) 
  | x==y        = 1 + mLen xs ys
  | otherwise   = max (mLen xs (y:ys)) (mLen (x:xs) ys)

-- ... translated to talk about sub-components of lists, described by their
-- endpoints ...

maxLen :: [Int] -> [Int] -> Int -> Int -> Int

maxLen xs ys 0 j = 0 
maxLen xs ys i 0 = 0
maxLen xs ys i j
  | xs!!(i-1) == ys!!(j-1)  = (maxLen xs ys (i-1) (j-1)) + 1
  | otherwise               = max (maxLen xs ys i (j-1))
                                  (maxLen xs ys (i-1) j)

-- ... and then transliterated into a memoised version.

maxTab ::  [Int] -> [Int] -> [[Int]]

maxTab xs ys
  = result
    where 
    result = [0,0 .. ] : zipWith f [0 .. ] result
    f i prev  
        = ans
          where
          ans   = 0 : zipWith g [0 .. ] ans
          g j v 
            | xs!!i == ys!!j      = prev!!j + 1
            | otherwise           = max v (prev!!(j+1))


