module Stack where

newtype Stack a = Stack [a]
  deriving (Show)

------------------------------------------
push       :: Stack a -> a -> Stack a
pop        :: Stack a -> Maybe (a, Stack a)
top        :: Stack a -> Maybe a
isEmpty    :: Stack a -> Bool
emptyStack :: Stack a
size       :: Stack a -> Int
toStack    :: [a] -> Stack a
------------------------------------------

push (Stack xs) x = Stack (x:xs)

pop (Stack []) = Nothing
pop (Stack xs) = Just (head xs, Stack $ tail xs)

top (Stack []) = Nothing
top (Stack xs) = Just (head xs)

isEmpty (Stack xs) = null xs

emptyStack = Stack []

size (Stack xs) = length xs

toStack xs = Stack xs
