module Notes3 where
import Prelude hiding (iterate)

-------------------------------
-- SECTION 17.6: INFINITE LISTS
-------------------------------

-- Lazy evaluation allows computations over descriptions of infinite structures
-- by only evaluating portions of the data structure rather than the whole object.

-- A simple example

ones :: [Int]
ones = 1 : ones

addFirstTwo :: [Int] -> Int
addFirstTwo (x:y:zs) = x+y

-- An evaluation:
-- addFirstTwo ones --> addFirstTwo (1:ones) --> addFirstTwo (1:1:ones) --> 1+1 --> 2

-- Another fun example

from :: Int -> [Int]
from n = n : from (n+1)

fromStep :: Int -> Int -> [Int]
fromStep n m = n : fromStep (n+m) m

-- List comprehensions can also define infinite lists

pythagTriples = [(x,y,z) | z<-[2..], y<-[2..z-1], x<-[2..y-1], x*x+y*y==z*z]

powers n = [n^x | x<-[0..]]

-- powers is a special case of the prelude iterate, which gives the infinite list:
-- [x, f x, ..., f^n x, ...]

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

fac n = if n <= 1 then 1 else n * fac (n-1) -- iterate fac n == bad idea!

-- Generating prime numbers! (This is great, I think my next problem on projectEuler
-- involves large primes

-- Prime number == one that is divisible only by itself and one.
-- The Sieve of Eratosthenes == works by cancelling out all multiples of numbers once they
-- are established as prime.

primes :: [Integer]
primes = sieve [2 ..]

sieve (x:xs) = x : sieve [y | y<-xs, mod y x > 0]

-- An example of evaluation of this list
-- primes --> sieve [2..] --> 2: sieve [y | y<-[3..], mod y 2 > 0] -->
-- 2: sieve (3: [y | y<-[4..], mod y 4 > 0]) --> 2:3: sieve [z | z<- [y | y<-[4..], mod y 2 > 0], mod z 3 > 0] -->
-- --> ... -->
-- 2 : 3 : sieve [z | z<-[5,7,9...], mod z 3 > 0] --> ... --> 2 : 3 : sieve [5,7,11,...] --> ...

-- We cannot test for non-membership of primes just yet, using the prelude member function,
-- it will never return False on a non-prime number because it will continue to check every number.
-- This is easy to fix...

memberOrd :: Ord a => [a] -> a -> Bool
memberOrd (x:xs) n
  | x<n        = memberOrd xs n
  | x==n       = True
  | otherwise  = False

------------------------------------
-- SECTION 17.7: WHY INFINITE LISTS?
------------------------------------

-- An infinite version of a program can be more abstract, so simpler to write.

-- This is another version of runningSums that was in the exercises

listSums :: [Integer] -> [Integer]
listSums xs = out
  where
    out = 0: zipWith (+) xs out

-- An example of this function being evaluated

-- listSums [1..] --> out --> 0: zipWith (+) [1..] out --> 0: zipWith (+) [1..] (0:...) -->
-- 0: 1+0 : zipWith (+) [2..] (1+0,...) --> 0:1:2+1: zipWith (+) [3..] (2+1:...) --> ....

-- Where the occurences of 'out' are replaced by incomplete lists.

-- This is an example of the general function scanl1', which combines values using the function f,
-- and whose output is st:

scanl1' :: (a -> b -> b) -> b -> [a] -> [b]
scanl1' f st iList = out
  where
    out = st : zipWith f iList out

-- Therefore, listSums = scanl1' (+) 0.

-- A function to keep a running sort:

--sorts = scanl1' ins [] -- where ins inserts an elemennt in an appropriate place in a sorted list

-- And a function to list factorials... Instead of mapping a factorial function on each number, it
-- keeps a running multiplication, lots quicker:

facs = scanl1' (*) 1 [1..]

-- The prelude function scanl has the same output as scanl1', but is implemented slightly different,
-- in order to keep with the theme of running sums

--facs = scanl (*) 1 [1..]

---------------------------------
-- SECTION 17.9: PROOFS REVISITED
---------------------------------

-- In nearly every programming language, it is possible to write a program which fails to
-- to terminate. The value of such programs is called an Undefined value.

-- For example:

undef :: a
undef = undef

-- which gives a non-terminating or undefined value of every type. Another example is:

fak n = (n+1) * fak n

-- The existence of undefined values has an effect on the type of lists:

list1 = 2:3:undef

-- which has a well-defined head, and it's tail also has a well-defined head, but the tail
-- is undefined.

-- Lazy evaluation still works for undefined values:
-- const 1 undef --> 1

-- But if the function applied to undef has to pattern match, then the result of the function
-- will be undef.
-- sum undef --> undef

-- An integer is defined if it is not equal to undef. A list is defined if it is a finite
-- list of defined values. Note that a finite list, as it is defined, may contain
-- undefined values.

-- How do we prove properties of infinite lists? We think of an approximate of the infinite list
-- as the list:
-- a0:a1:a2:...:an:undef.

-- In other words, we say that the partial lists:
-- undef, a0:undef, a0:a1:undef, a0:a1:a2:undef, ...
-- are approximations to the infinite list [a0,a1,a2,...,an,...]

-- A note on infinte list equality:

-- A list xs is infinite if for all natural numbers n, take n xs /= take (n+1) xs
-- Two infinite lists xs and ys are equal if for all natural numbers n, xs!!n == ys!!n

-- If a result holds for all finite partial lists, then it holds for all approximates
-- to infinite lists. For some properties, it is enough to know the property
-- for all approximates to know that it will be valid for all infinite lists as well.
-- In particular, this is true for all equations. This means that, we can assert
-- that for ALL lists xs: (map f . map g) xs = map (f.g) xs
-- and therefore, by the principle of extensibility: map f . map g = map (f.g)
