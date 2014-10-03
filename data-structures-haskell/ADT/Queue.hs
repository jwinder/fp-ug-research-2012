module Queue where

newtype Queue a = Queue [a]
  deriving (Show)

------------------------------------------
enqueue    :: Queue a -> a -> Queue a
dequeue    :: Queue a -> Maybe (a, Queue a)
front      :: Queue a -> Maybe a
back       :: Queue a -> Maybe a
isEmpty    :: Queue a -> Bool
emptyQueue :: Queue a
size       :: Queue a -> Int
toQueue    :: [a] -> Queue a
------------------------------------------

enqueue (Queue xs) x = Queue (xs ++ [x])

dequeue (Queue []) = Nothing
dequeue (Queue xs) = Just (head xs, Queue $ tail xs)

front (Queue []) = Nothing
front (Queue xs) = Just (head xs)

back (Queue []) = Nothing
back (Queue xs) = Just (last xs)

isEmpty (Queue xs) = null xs

emptyQueue = Queue []

size (Queue xs) = length xs

toQueue xs = Queue xs
