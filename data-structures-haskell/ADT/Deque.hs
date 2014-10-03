module Deque where

newtype Deque a = Deque [a]
  deriving (Show)

------------------------------------------
addFront    :: Deque a -> a -> Deque a
addBack     :: Deque a -> a -> Deque a
removeFront :: Deque a -> Maybe (a, Deque a)
removeBack  :: Deque a -> Maybe (a, Deque a)
front       :: Deque a -> Maybe a
back        :: Deque a -> Maybe a
isEmpty     :: Deque a -> Bool
emptyDeque  :: Deque a
size        :: Deque a -> Int
toDeque     :: [a] -> Deque a
------------------------------------------

addFront (Deque xs) x = Deque (x:xs)

addBack (Deque xs) x = Deque (xs ++ [x])

removeFront (Deque []) = Nothing
removeFront (Deque xs) = Just (head xs, Deque $ tail xs)

removeBack (Deque []) = Nothing
removeBack (Deque xs) = Just (last xs, Deque $ init xs)

front (Deque []) = Nothing
front (Deque xs) = Just (head xs)

back (Deque []) = Nothing
back (Deque xs) = Just (last xs)

isEmpty (Deque xs) = null xs

emptyDeque = Deque []

size (Deque xs) = length xs

toDeque xs = Deque xs
