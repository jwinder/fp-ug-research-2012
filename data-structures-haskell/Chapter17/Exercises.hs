{-
  Chapter 17 Exercises
-}
module Exercises where
import List

type Vector = [Float]
type Matrix = [Vector]

-------
-- 17.1
-------

-- Give a calculation of the expression [x+y | x<-[1..4], y<-[2..4], x>y]

-- [1+y | y<-[2..4], 1>y] ++ [2+y | y<-[2..4], 2>y] ++ [3+y | y<-[2..4], 3>y] ++ [4+y | y<-[2..4], 4>y] -->

-- [1+2 | 1>2] ++ [1+3 | 1>3] ++ [1+4 | 1>4] ++
-- [2+2 | 2>2] ++ [2+3 | 2>3] ++ [2+4 | 2>4] ++
-- [3+2 | 3>2] ++ [3+3 | 3>3] ++ [3+4 | 1>4] ++
-- [4+2 | 4>2] ++ [4+3 | 4>3] ++ [4+4 | 4>4] -->

-- [3 | False] ++ [4 | 1>3] ++ [5 | False] ++
-- [4 | False] ++ [5 | False] ++ [6 | False] ++
-- [5 | True] ++ [6 | False] ++ [7 | False] ++
-- [6 | True] ++ [7 | True] ++ [8 | False] -->

-- [5] ++ [6] ++ [7] --> [5,6,7]

-------
-- 17.2
-------

-- Using list comprehensions, define the functions

--subLists, subSequences :: [a] -> [[a]]

-- which return all sublists and subsequences of a list.

-- A sublist == list that omits at least one element from the original list
-- A subsequence == ordered continuous block from a list

-- Example, [2,4] and [3,4] are sublists of [2,3,4] but [3,4] is a sequence while [2,4] is not

-- I DIDN'T GET TO THIS ONE...

-------
-- 17.4
-------

-- Give a definition of scalarProduct using zipWith.

scalarProduct :: Vector -> Vector -> Float
scalarProduct xs ys = sum (zipWith (*) xs ys)

-------
-- 17.5
-------

-- Define functions to calculate the determinant of a square matrix and to invert a matrix.
{-
detMat :: Matrix -> Maybe Float
invertMat :: Matrix -> Maybe Matrix

detMat m
  | length m == 0              = Nothing
  | and (map (length m ==) m)  = Just (det m)
  | otherwise                  = Nothing

det :: Matrix -> Float
det [[x]] = x
det m = basketWeave longMat - basketWeave (reverse longMat)
  where
    longMat = zipWith (++) m m

basketWeave m
  | length m == 0  = 1
--  | head (head m) *
-}

-- DIDN'T GET THIS ONE FINISHED YET EITHER...

-------
-- 17.8
-------

-- Redefine the following list comprehensions using Prelude functions.

-- [m*m | m<-[1..10]] --> map (^2) [1..10]
-- [m*m | m<-[1..10], m*m<50] --> (filter (<50) . map (^2) [1..10]
