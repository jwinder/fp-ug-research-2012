{-
  Chapter 17 :: Lazy Programming
  This is code from the reading, not the exercises.
-}

module Notes where
import List

fibs = 0:1: [a+b | (a,b)<-zip fibs (tail fibs)]
fib n = fibs!!n

--------------------------------
-- SECTION 17.1: LAZY EVALUATION
-------------------------------- 

-- Lazy Evaluation: A lazy evaluator will only evaluate an argument to a
-- function if that argument's value is needed to compute the overall result.

-- A note on function evaluation

f x y = x+y

-- f (9-3) (f 34 3) gives (9-3)+(f 34 3).
-- The expressions are not evaluated before they are passed
-- to the function. But, for the evaluation to continute, they arguments
-- will be evaluated.

g x y = x+12

-- g (9-3) (g 34 3) gives (9-3)+12 --> 5+3 --> 18.
-- The y does not appear in the equation so the argument (g 34 3) will not
-- be evaluated.

-- Another example, if n>0 then y is not evaluated. Otherwise x is not evaluated.

switch :: Int -> a -> a -> a
switch n x y
  | n>0        = x
  | otherwise  = y

-- Lazy evaluation also always ensures duplicate arguments are never evaluated
-- more than once. The calculation is done by corresponding steps simultaneously.

h x y = x+x

-- h (9-3) (h 34 3) gives (9-3)+(9-3) --> 6+6 --> 12

-- Final example

pm (x,y) = x+1

-- pm (3+2,4-17) --> (3+2)+1 --> 6

------------------------------------------------------
-- SECTION 17.2: CALCULATION RULES AND LAZY EVALUATION
------------------------------------------------------

-- A note about pattern matching

-- f p1 p2 ... pk
--   | g1 = e1
--   | g2 = e2
--   | otherwise = er
-- f q1 q2 ... qk
--   = ...

-- The arguments of a function are not evaluated fully.
-- Rather, they are evaluated sufficiently until a corresponding
-- pattern is matched in the first equation. If a pattern is not found, then
-- the second equation is tested. This is repeated until a match is given.
-- If a pattern is not found, then a program error is generated.

addFirst :: [Int] -> [Int] -> Int
addFirst [] ys         = 0             -- (*1)
addFirst (x:xs) []     = 0             -- (*2)
addFirst (x:xs) (y:ys) = x+y           -- (*3)

-- addFirst [1..3] [1..3]              (1)
-- --> addFirst (1:[2..3]) [1..3]      (2)
-- --> addFirst (1:[2..3]) (1:[2..3])  (3)
-- --> 1+1                             (4)

-- At stage 1, there is not enough information about the arguments to determine
-- whether there is a match *1. One step of evaluation gives 2 and shows there is
-- not a match with *1. The first argument of 2 matches the first pattern of *2,
-- so check the second argument. One step calculation in 3 shows that there is
-- no match with *2, but that there is one with *3, hence we get 4.

-- With regard to calculation using guards, the clause on the right-hand-side of
-- the equal sign is only applied when the condition on the left-hand-side
-- is evalued to true. The value corresponding to the first guard evaluated as
-- true is used.

-- Values in where clauses are only calculated on demand.

-- Operators and other expression formers

-- Example, given an operator:
-- True && x = x
-- False && x = False

-- (&&) will not evaluated its second argument if the first argument is False.

-- Other operators vary. (+) needs both arguments, while equality on lists
-- does not. In general, the language is implemented so that no unnecessary
-- evaluation takes place.

-- Note that:
-- if..then..else statements are evaluated like a guard
-- case statements are evaluated like a pattern match
-- let is evaluated like a where clause
-- a lambda expression is evaluated like the application of a named function

-- A note on evaluation order

-- Evaluation is FROM THE OUTSIDE IN, otherwise it is from LEFT TO RIGHT
-- i.e. f e (g e 17) evaluates f e () and then evaluates g e 17
-- f e + g e evaluates f e then g e

----------------------------------------------
-- SECTION 17.3: LIST COMPREHENSIONS REVISITED
----------------------------------------------

-- A list comprehension has the form [ e | q1 , ... , qk ] where each quantifier
-- is either a generator (i.e. p<-ps) or a test (i.e. p>0). An expression appearing
-- in quantifer qi can refer to variables used in the patterns of the previous
-- quantifiers, q1 to q(i-1).

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = [(x,y) | x<-xs,y<-ys]

-- pairs [1,2,3] [4,5] --> [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]
-- The first element of xs, 1, is given to x, and then for this fixed value, all
-- possible values of y in ys are chosen. This repeated for x values of 2 and 3.

triangle :: Int -> [(Int,Int)]
triangle n = [(x,y) | x<-[1..n], y<-[1..x]]

-- triangle 3 --> [(1,1),(2,1),(2,2),(3,1),(3,2),(3,3)]
-- The second generator, y, depends on the value of x given by the first generator.

-- A cool example that tests three generators!
pyTrippio n = [(x,y,z) | x<-[2..n], y<-[x+1..n], z<-[y+1..n], x*x+y*y==z*z]

-- The evaluation of list comprehensions are a bit lengthy, so I'm only including this one...
-- triangle 3 --> [(x,y) | x<-[1..3], y<-[1..x]] -->
--            [(1,y) | y<-[1..1]]  ++ [(2,y) | y<-[1..2]] ++ [(3,y) | y<-[1..3]] -->
--            [(1,1) |] ++ [(2,1) |] ++ [(2,2) |] ++ [(3,1) |] ++ [(3,2) |] ++ [(3,3) |] -->
--            [(1,1),(2,1),(2,2),(3,1),(3,2),(3,3)]

-- A few other neat examples. Not copying the entire evaluations, they take too long

perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [x:ps | x<-xs, ps<-perms (xs\\[x])] -- (\\) is a set difference, in the module List, and requires Eq a

-- Another permutations implementation, that doesn't require Eq a

perm :: [a] -> [[a]]
perm [] = [[]]
perm (x:xs) = [ ps++[x]++qs | rs<-perm xs, (ps,qs)<- splits rs ]

splits :: [a] -> [( [a], [a] )]
splits [] = [( [], [] )]
splits (y:ys) = ([],y:ys) : [(y:ps,qs) | (ps,qs)<- splits ys]

-- Vectors and Matrices!

type Vector = [Float]

-- If we want a scalar product function, notice that the function:
-- scalarProd xs ys = sum [x*y | x<-xs, y<-ys]
-- returns the sum of all combinations, not only the pairwise combinations.

scalarProd :: Vector -> Vector -> Float
scalarProd xs ys = sum [x*y | (x,y)<- zip xs ys]
--scalarProd xs ys = sum (zipWith (*) xs ys)

-- This approach represents a matrix as a list of rows
type Matrix = [Vector]

matrixProd :: Matrix -> Matrix -> Matrix
matrixProd m p = [ [scalarProd r c | c <- columns p] | r<-m ]

-- Transpose, a matrix as a list of columns
columns :: Matrix -> Matrix
columns y = [ [z!!j | z<-y] | j<-[0..s] ]
  where
    s = length (head y)-1
--columns = transpose -- tranpose does the same thing, a module in List

-- Some patterns are refutable. An attempt to pattern-match against them may fail.
-- For example,
-- [x | (x:xs) <- [[],[2],[],[4,5]]] --> [2,4]
-- The list is still left with the elements that match the pattern

------------------------------------------
-- SECTION 17.4: DATA-DIRECTED PROGRAMMING
------------------------------------------

-- Data-directed programming: a style of programming in which complex data structures are
-- constructed and manipulated (often on demand).

-- Example of a data-directed solution to finding the sum of fourth poewrs of numbers from
-- 1 to n:
-- 1. Build a list [1..n]
-- 2. Take the poewr of each number, giving [1,16,...,n^4]
-- 4. Find the sum of this list

sumFourthPowers n = sum (map (^4) [1..n])
-- sumFourthPowers n = (sum . map (^4)) [1..n]
