{-
    FirstScript.hs

    An example of a multiline comment.
-}

module FirstScript where

-- The value size is an integer (Int), defined to be the sum of twelve and thirteen.

size :: Int
size = 12+13

-- The function to square an integer.

square :: Int -> Int
square n = n*n

-- The function to double an integer.

double :: Int -> Int
double n = 2*n

-- An example using double, square and size.

example :: Int
example = double (size - square (2+2))

-- The function to square the double of an integer.

doubleThenSquare :: Int -> Int
doubleThenSquare = square . double 

-- The function to double the square of an integer.

squareThenDouble :: Int -> Int
squareThenDouble = double . square
