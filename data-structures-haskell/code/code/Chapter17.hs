
-- 	Haskell: The Craft of Functional Programming
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1999.

-- 	Chapter 17

-- Lazy programming
-- ^^^^^^^^^^^^^^^^

{- helium changes
include iSort, specialised to [Int], from Chapter 19
to replace \\ define a difference and subtract function which take
explicit eq arguments, too
It appears that cannot import from a module in another directory ...
also the Set implementation uses type classes heavily.
Comment out the import of Set and Relation, plus the code that uses them.  
Set will require wholesale rewrite.
Change type Double to Float
replace + by +. over Float, and define sumFloat to act as sum over Float.
ditto for > and * i.e. replace by >. and *.
-}

module Chapter17 where

-- import List hiding (union)		-- for \\
-- import Chapter19 hiding (map,fac)	-- for iSort
-- import Set				-- for Relation
-- import Relation				-- for graphs

-- Lazy evaluation
-- ^^^^^^^^^^^^^^^

-- Some example functions illustrating aspects of laziness.

f x y = x+y

g x y = x+12

switch :: Int -> a -> a -> a
switch n x y
  | n>0         = x
  | otherwise   = y

h x y = x+x

pm (x,y) = x+1


-- Calculation rules and lazy evaluation
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Some more examples.

f1 :: [Int] -> [Int] -> Int
f1 [] ys         = 0 
f1 (x:xs) []     = 0 
f1 (x:xs) (y:ys) = x+y 

f2 :: Int -> Int -> Int -> Int
f2 m n p
  | m>=n && m>=p        = m
  | n>=m && n>=p        = n
  | otherwise           = p

f3 :: Int -> Int -> Int

f3 a b
  | notNil xs    = front xs
  | otherwise    = b
    where
    xs = [a .. b]

front (x:y:zs) = x+y
front [x]      = x

notNil []    = False
notNil (_:_) = True



-- List comprehensions revisited
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Simpler examples
-- ^^^^^^^^^^^^^^^^

-- All pairs formed from elements of two lists

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = [ (x,y) | x<-xs , y<-ys ]

pairEg = pairs [1,2,3] [4,5] 

-- Illustrating the order in which elements are chosen in multiple
-- generators.

triangle :: Int -> [(Int,Int)]
triangle n = [ (x,y) | x <- [1 .. n] , y <- [1 .. x] ]

-- Pythagorean triples

pyTriple n
  = [ (x,y,z) | x <- [2 .. n] , y <- [x+1 .. n] , 
                z <- [y+1 .. n] , x*x + y*y == z*z ]


-- Calculating with list comprehensions
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- The running example from this section.

runningExample = [ x+y | x <- [1,2] , isEven x , y <- [x .. 2*x] ]

isEven :: Int -> Bool
isEven n = (n `mod` 2 == 0)


-- List permutations
-- ^^^^^^^^^^^^^^^^^


-- One definition of the list of all permutations.
-- For this to work need eq over a
-- to replace \\ define a difference and subtract function which take
-- explicit eq arguments, too

perms :: (a -> a -> Bool) -> [a] -> [[a]]

perms eq [] = [[]]
perms eq xs = [ x:ps | x <- xs , ps <- perms eq (diff eq xs [x]) ]

diff :: (a -> a -> Bool) -> [a] -> [a] -> [a]

diff eq [] _ = []
diff eq xs [] = xs
diff eq xs (y:ys) = diff eq (sub eq xs y) ys

sub :: (a -> a -> Bool) -> [a] -> a -> [a] 

sub eq [] y = []
sub eq (x:xs) y 
  | eq x y 	= xs
  | otherwise 	= x : sub eq xs y

-- Another algorithm for permutations

perm :: [a] -> [[a]]

perm []     = [[]]
perm (x:xs) = [ ps++[x]++qs | rs <- perm xs ,
                              (ps,qs) <- splits rs ]

-- All the splits of a list into two halves.

splits :: [a]->[([a],[a])]

splits []     = [ ([],[]) ]
splits (y:ys) = ([],y:ys) : [ (y:ps,qs) | (ps,qs) <- splits ys]



-- Vectors and Matrices
-- ^^^^^^^^^^^^^^^^^^^^


-- A vector is a sequence of real numbers, 

type Vector = [Float]

-- and the scalar product of two vectors.

scalarProduct :: Vector -> Vector -> Float
scalarProduct xs ys = sumFloat [ x*.y | (x,y) <- zip xs ys ]

sumFloat = foldr (+.) 0.0

-- The type of matrices.

type Matrix = [Vector]

-- and matrix product.

matrixProduct :: Matrix -> Matrix -> Matrix
matrixProduct m p
  = [ [scalarProduct r c | c <- columns p] | r <- m ]

-- where the function columns gives the representation of a matrix as a
-- list of columns.

columns :: Matrix -> Matrix

columns y = [ [ z!!j | z <- y ] | j <- [0 .. s] ]
            where 
            s = length (head y)-1


-- Refutable patterns: an example
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

refPattEx = [ x | (x:xs) <- [[],[2],[],[4,5]] ]



-- Data-directed programming
-- ^^^^^^^^^^^^^^^^^^^^^^^^^

-- Summing fourth powers of numbers up to n.

sumFourthPowers :: Int -> Int
sumFourthPowers n = sum (map ((^)4) [1 .. n])

-- List minimum: take the head of the sorted list. Only makes sense in an
-- lazy context.

minList :: [Int] -> Int

minList = head . iSort

iSort :: [Int] -> [Int]

iSort []     = []
iSort (x:xs) = ins x (iSort xs)

ins :: Int -> [Int] -> [Int]

ins x [] = [x]
ins x (y:ys) 
  | (x<=y)      = x:y:ys
  | otherwise   = y:ins x ys

{- comment out for helium

-- Example: routes through a graph
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- A example graph.

graphEx = makeSet [(1,2),(1,3),(2,4),(3,5),(5,6),(3,6)]

-- Look for all paths from one point to another. (Assumes the graph is acyclic.)

routes :: Ord a => Relation a -> a -> a -> [[a]]

routes rel x y
  | x==y        = [[x]]
  | otherwise   = [ x:r | z <- nbhrs rel x ,
                          r <- routes rel z y ]
-- 	
-- The neighbours of a point in a graph.

nbhrs :: Ord a => Relation a -> a -> [a]
nbhrs rel x = flatten (image rel x)

-- Example evaluations

routeEx1 = routes graphEx 1 4

routeEx2 = routes graphEx 1 6


-- Accommodating cyclic graphs.

routesC :: Ord a => Relation a -> a -> a -> [a] -> [[a]]
routesC rel x y avoid
  | x==y        = [[x]]
  | otherwise   = [ x:r | z <- nbhrs rel x \\ avoid ,
                          r <- routesC rel z y (x:avoid) ]
-}

-- Case study: Parsing expressions
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- See under case studies for parsing and the calculator..



-- Infinite lists
-- ^^^^^^^^^^^^^^

-- The infinite list of ones.

ones :: [Int]
ones = 1 : ones

-- Add the first two elements of a list.

addFirstTwo :: [Int] -> Int
addFirstTwo (x:y:zs) = x+y

-- Example, applied to ones.

infEx1 = addFirstTwo ones

-- Arithmetic progressions

from :: Int -> [Int]
from n       = n : from (n+1)

fromStep :: Int -> Int -> [Int]
fromStep n m = n : fromStep (n+m) m

-- and an example.

infEx2 = fromStep 3 2

-- Infinite list comprehensions.

-- Pythagorean triples

pythagTriples =
 [ (x,y,z) | z <- [2 .. ] , y <- [2 .. z-1] , 
             x <- [2 .. y-1] , x*x + y*y == z*z ]

-- The powers of an integer 

powers :: Int -> [Int]
powers n = [ n^x | x <- [0 .. ] ] 

-- Iterating a function (from the Prelude)

-- 	iterate :: (a -> a) -> a -> [a]
-- 	iterate f x = x : iterate f (f x)

-- Sieve of Eratosthenes

primes :: [Int]

primes       = sieve [2 .. ]
sieve (x:xs) = x : sieve [ y | y <- xs , y `mod` x > 0]

-- Membership of an ordered list.

{-
memberOrd :: Ord a => [a] -> a -> Bool
memberOrd (x:xs) n
  | x<n         = memberOrd xs n
  | x==n        = True
  | otherwise   = False
-}

-- Example: Generating random numbers
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Find the next (pseudo-)random number in the sequence.

nextRand :: Int -> Int
nextRand n = (multiplier*n + increment) `mod` modulus

-- A (pseudo-)random sequence is given by iterating this function,

randomSequence :: Int -> [Int]
randomSequence = iterate nextRand

-- Suitable values for the constants.

seed, multiplier, increment, modulus :: Int

seed       = 17489
multiplier = 25173
increment  = 13849
modulus    = 65536

-- Scaling the numbers to come in the (integer) range a to b (inclusive).

scaleSequence :: Int -> Int -> [Int] -> [Int]
scaleSequence s t
  = map scale
    where
    scale n = n `div` denom + s
    range   = t-s+1
    denom   = modulus `div` range

-- Turn a distribution into a function.

makeFunction :: [(a,Float)] -> (Float -> a)

makeFunction dist = makeFun dist 0.0

makeFun ((ob,p):dist) nLast rand
  | nNext >=. rand && rand >. nLast     
        = ob
  | otherwise                           
        = makeFun dist nNext rand
          where
          nNext = p*.fromInt modulus +. nLast

-- Random numbers from 1 to 6 according to the example distribution, dist.

randomTimes = map (makeFunction dist . fromInt) (randomSequence seed)

-- The distribution in question

dist = [(1,0.2), (2,0.25), (3,0.25), (4,0.15), (5,0.1), (6,0.05)]



-- A pitfall of infinite list generators
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- An incorrect Pythagorean triples program.

pythagTriples2
  = [ (x,y,z) | x <- [2 .. ] ,
                y <- [x+1 .. ] ,
                z <- [y+1 .. ] ,
                x*x + y*y == z*z ]


-- Why infinite lists?
-- ^^^^^^^^^^^^^^^^^^^

-- Running sums of a list of numbers.

listSums :: [Int] -> [Int]

listSums iList = out
                 where
                 out = 0 : zipWith (+) iList out

-- We give a calculation of an example now.

listSumsEx = listSums [1 .. ]

-- Another definition of listSums which uses scanl1', a generalisation of the
-- original function.

listSums' = scanl1' (+) 0

-- A function which combines values from the list
-- using the function f, and whose first output is st.

scanl1' :: (a -> b -> b) -> b -> [a] -> [b]
scanl1' f st iList
  = out
    where
    out = st : zipWith f iList out

-- Factorial Values

facVals = scanl1' (*) 1 [1 .. ]



-- Case study: Simulation
-- ^^^^^^^^^^^^^^^^^^^^^^

-- See case studies.



-- Two factorial lists
-- ^^^^^^^^^^^^^^^^^^^

-- The factorial function 

fac :: Int -> Int

fac 0 = 1
fac m = m * fac (m-1)
-- 	
-- Two factorial lists

facMap, facs :: [Int]

facMap = map fac [0 .. ]
facs = 1 : zipWith (*) [1 .. ] facs
