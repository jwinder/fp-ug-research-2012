{-
  Chapter 12: Overloading and type classes
-}

module Tester where

import Prelude hiding (elem)
import Char

f .> g = g . f

elemBool :: Bool -> [Bool] -> Bool
elemBool x [] = False
elemBool x (y:ys) = (x==y) || elemBool x ys

elemInt :: Int -> [Int] -> Bool
elemInt x [] = False
elemInt x (y:ys) = (x==y) || elemInt x ys

-- The above functions are identical except for the types...

elem :: Prelude.Eq a => a -> [a] -> Bool
elem x [] = False
elem x (y:ys) = (x==y) || elem x ys

-- The above function has type a->[a]->Bool and only holds true for types
-- that have an equality function, how do we define a class, such as Eq
-- that has equality?

-- We need Eq to have (==)

--class Eq a where
--  (==) :: a -> a -> Bool

allEqual :: Int -> Int -> Int -> Bool
allEqual a b c = a==b && b==c

-- allEqual is not really specific to integers, it can use any type that has
-- an equality assigned to it

-- Eq => a is the 'context' in which a is used, meaning that a is of type class Eq

allEquall :: Prelude.Eq a => a -> a -> a -> Bool
allEquall x y z = x==y && y==z

-- allEquall suc suc suc would return Int -> Int not an instance of class Eq
-- because equality cannot be inferred upon functions...

suc :: Int -> Int
suc = (+1)

numEqual x xs = length [y | y<-xs, y==x]
numEquall x = filter (==x) .> length

-- Declaring a class

-- Visible is the name of the class, and then follows is the signature
-- The signature is the list of names and their types
-- Any type a in the Visible class must carry the two functions in the signature,
-- that is, the functions toString and size

class Visible a where
  toString :: a -> String
  size     :: a -> Int

-- The general form of the class definition is
-- class Name ty where
--   ... signature involving the variable ty ...

-- A type is a made member or instance of a class by defining the signatures for
-- the type, for example,
-- instance Eq Bool where
--   True == True = True
--   False == True = True
--   ...
-- This describes how Bool is an instance of the equality class.

-- The definitions in the instance can be specific to whichever type you want to define

instance Visible Char where
  toString ch = [ch]
  size _ = 1

instance Visible Bool where
  toString True = "True"
  toString False = "False"
  size _ = 1

--instance Visible Int where
--  toString = show
--  size = show .> length

-- Given a list of values over type a, we can use toString and size to define
-- the functions over [a]

instance Visible a => Visible [a] where
  toString = map toString .> concat
  size = map size .> foldr (+) 1

qSort [] = []
qSort (x:xs) = qSort [y | y<-xs,y<=x] ++ [x] ++ qSort [y | y<-xs,y>x]

-- vSort [a] must be orderable and Visible in order to be converted to a string,
-- So make the context of a both type classes
vSort :: (Ord a, Visible a) => [a] -> String
vSort = qSort .> toString

class (Ord a, Visible a) => OrdVis a

-- In this situation, a utilizes multiple inheritance from Ord and Visible, which is
-- to be explored more in chater 14

-- This doesn't work yet, Ord has a compare function in its signature as well
--compare :: OrdVis a => a -> a -> Bool
--compare x y = size x <= size y

showBoolFun :: (Bool -> Bool) -> String
showBoolFun f = "b | f(b)\nT | " ++ show (f True) ++ "\nF | " ++ show (f False)
