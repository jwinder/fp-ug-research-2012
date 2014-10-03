module Notes2 where
import Char
import Data.Set

-- 17.4 Continued

graphEx = fromList [(1,2),(1,3),(2,4),(3,5),(5,6),(3,6)]

-- Case study for parsing expressions, that is a function that takes a list
-- of objects, of type a, and from it extract another type, b.

infixr 5 >*> -- Sets precedence level to 5

type Parse a b = [a] -> [(b,[a])]

-- A parser which always fails

none :: Parse a b
none inp = []

-- An always successful parser with the value being a parameter of the function

succeed :: b -> Parse a b
succeed val inp = [(val,inp)]

-- More useful is a parser to recognize a single object, or token

--token :: Eq a => a -> Parse a a
--token t (x:xs)
--  | t==x       = [(t,xs)]
--  | otherwise  = []
--token t []     = []

-- More generally, we can recognize objects with a particular property,
-- as represented by a boolean function

spot :: (a -> Bool) -> Parse a a
spot p (x:xs)
  | p x        = [(x,xs)]
  | otherwise  = []
spot p []      = []

-- Examples

bracket = token '('
dig = spot isDigit

-- Can redefine token from spot:
token t = spot (==t)

-- alt will combine the results from two parses into a single parse

alt :: Parse a b -> Parse a b -> Parse a b
alt p1 p2 inp = p1 inp ++ p2 inp

(>*>) :: Parse a b -> Parse a c -> Parse a (b,c)
(>*>) p1 p2 inp = [((y,z),rem2) | (y,rem1)<- p1 inp, (z,rem2)<- p2 rem1]

build :: Parse a b -> (b -> c) -> Parse a c
build p f inp = [(f x,rem) | (x,rem) <- p inp]

list :: Parse a b -> Parse a [b]
list p = (succeed []) `alt` ((p >*> list p) `build` (uncurry (:)))
