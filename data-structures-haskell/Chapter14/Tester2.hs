{-
  Chapter 14: Algebraic Types
  (Second half of the chapter)
-}
module Tester2 where

import Prelude hiding (tail,maybe)

f .> g = g . f

-- Handling program errors

-- The following function stops program executation and reports the problem
-- to the default output device.
--error :: String -> a

-- Dummy values

-- The tail function is supposed to give the tail of a list and a default
-- error message on an empty list
tail :: [a] -> [a]
tail (_:xs) = xs
tail [] = error "PreludeList.tail: empty list"

-- We could redefine it as...
tl :: [a] -> [a]
tl (_:xs) = xs
tl [] = []

-- Similarly, we could return 0 for division by zero, a somewhat logical choice

-- We can also supply an extra argument for the error cases
hd :: a -> [a] -> a
hd y (x:_) = x
hd y [] = y

-- This approach is generalizable
{-
  fErr y x
    | cond = y
    | otherwise = f x
-}

-- Error types
{-
-- Effectively just the type a with an extra value Nothing added (this is prelude)

data Maybe a = Nothing | Just a
  deriving (Eq,Ord,Show,Read)
-}

errDiv :: Int -> Int -> Maybe Int
errDiv n m
  | m/=0       = Just (div n m)
  | otherwise  = Nothing

{-
-- General Error case

fErr x
  | cond       = Nothing
  | otherwise  = Just (f x)
-}

-- Therefore, the error types are not of the original output type, but of the Maybe a type.
-- We can retransmit the error through a function, like mapMaybe
-- or we can trap the error using the function maybe

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe g Nothing = Nothing
mapMaybe g (Just x) = Just (g x)

maybe :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing = n
maybe n f (Just x) = f x

-- Example
{-
maybe 56 (1+) (mapMaybe (*3) (errDiv 9 0))
= maybe 56 (1+) (mapMaybe (*3) Nothing)
= maybe 56 (1+) Nothing
= 56

maybe 56 (1+) (mapMaybe (*3) (errDiv 9 1))
= maybe 56 (1+) (mapMaybe (*3) (Just 9))
= maybe 56 (1+) (Just 27)
= 1 + 27
= 28
-}

-- Process takes elements n m from a list and returns their sum,
-- where the list is indexed by p^0,...,p^k and has k+1 elements 

errEle :: [Int] -> Int -> Maybe Int
errEle xs n
  | n < length xs  = Just (xs!!n)
  | otherwise      = Nothing

maybeToInt :: Maybe Int -> Int
maybeToInt (Just n) = n 

process :: [Int] -> Int -> Int -> Int
process xs n m = maybe 0 (maybeToInt (errEle xs n) +) (errEle xs m)

-- Function to compress Just (Just x) --> Just x

squashMaybe :: Maybe (Maybe a) -> Maybe a
squashMaybe (Just (Just x)) = Just x
squashMaybe _ = Nothing

-- Fun simple Edit type!

data Edit = Change Char | Copy | Delete | Insert Char | Kill
  deriving (Eq,Show)

-- The problem is to find the lowest-cost sequence of edits to go from
-- one string to the other. We begin with a definition like...

transform :: String -> String -> [Edit]
transform [] [] = []

-- To go from any string to [] we just kill it...

transform xs [] = [Kill]

-- To go from [] to st we just insert each character in its turn...

transform [] ys = map Insert ys

-- The general case

transform (x:xs) (y:ys)
  | x==y       = Copy : transform xs ys
  | otherwise  = best [ Delete   : transform xs (y:ys),
                        Insert y : transform (x:xs) ys,
                        Change y : transform xs ys ]

best :: [[Edit]] -> [Edit]
best [x] = x
best (x:xs)
  | cost x <= cost b  = x
  | otherwise         = b
  where 
    b = best xs

cost :: [Edit] -> Int
cost = filter (/=Copy) .> length

-- Movable objects!!!!

data Vector = Vec Float Float

class Movable a where
  move      :: Vector -> a -> a
  reflectX  :: a -> a
  reflectY  :: a -> a
  rotate180 :: a -> a
  rotate180 = reflectX . reflectY

data Point = Point Float Float
  deriving Show

instance Movable Point where
  move (Vec v1 v2) (Point c1 c2) = Point (c1+v1) (c2+v2)
  reflectX (Point c1 c2)  = Point c1 (-c2)
  reflectY (Point c1 c2)  = Point (-c1) c2
  rotate180 (Point c1 c2) = Point (-c1) (-c2)

data Figure = Line Point Point | Circle Point Float
  deriving Show

instance Movable Figure where
  move v (Line p1 p2) = Line (move v p1) (move v p2)
  move v (Circle p r) = Circle (move v p) r
  reflectX (Line p1 p2) = Line (reflectX p1) (reflectX p2)
  reflectX (Circle p r) = Circle (reflectX p) r
  reflectY (Line p1 p2) = Line (reflectY p1) (reflectY p2)
  reflectY (Circle p r) = Circle (reflectY p) r

instance Movable a => Movable [a] where
  move v = map (move v)
  reflectX = map reflectX
  reflectY = map reflectY

-- Named objects, ways to identify the name of a value...

class Named a where
  lookName :: a -> String
  giveName :: String -> a -> a

data Name a = Pair a String
  deriving Show

exam1 = Pair (Point 0.0 0.0) "Dweezil"

instance Named (Name a) where
  lookName (Pair obj nm) = nm
  giveName nm (Pair obj _) = (Pair obj nm)

-- Putting together classes

mapName :: (a -> b) -> Name a -> Name b
mapName f (Pair obj nm) = Pair (f obj) nm

instance Movable a => Movable (Name a) where
  move v = mapName (move v)
  reflectX = mapName reflectX
  reflectY = mapName reflectY

-- We have combined named and movable, so we can see that...

class (Movable b, Named b) => NamedMovable b

instance Movable a => NamedMovable (Name a)
