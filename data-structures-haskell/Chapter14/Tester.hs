{-

  Chapter 14 => Algebraic Types

-}
module Tester where

import Prelude hiding (either)

f .> g = g . f

-- Algebraic data types are introduced by the keyword 'data'

-- Enumerated types
data Temp = Cold | Hot
  deriving (Eq,Ord,Enum,Show,Read)

-- Or we can make instances for Temp, just a review of how to do it
{-instance Show Temp where
  show Cold = "Cold"
  show Hot  = "Hot"

instance Eq Temp where
  Cold == Cold = True
  Cold == Hot  = False
  Hot  == Cold = False
  Hot  == Hot  = True-}

data Season = Spring | Summer | Autumn | Winter
  deriving (Eq,Ord,Enum,Show,Read)

-- Cold and Hot are the 'constructors' of the type Temp

-- Pattern matching to define functions over enumerated types
weather :: Season -> Temp
weather Summer = Hot
weather _ = Cold

-- The built-in data types, commented out to avoid ambiguity annoyances
--data Bool = False | True
--data Ordering = LT | EQ | GT -- Ordering is used in class Ord

-- Product types, a type with a number of components
-- The type will need two values suppied, a name and age. The element of people
-- formatted from them will be Person st n. The 'Person' is kind of an identifier.
-- :type Person "yep" 19 :: People
data People = Person Name Age

type Name = String
type Age = Int

showPerson :: People -> String
showPerson (Person st n) = st ++ " -- " ++ show n

-- The type People has a single constructor, Person, and it is binary because
-- it takes in two elements to form the value of type People. For enumerated types,
-- Temp and Season for example, the constructors are nullary as they take no arguments
-- The constructors, such as Person st n, can be used just like functions, as
-- Person st n is the result of applying the function Person to arguments st and n.
-- Person :: Name -> Age -> People
-- Therefore, the type of a product type is defined as a function

-- This approach works well over unary constructors as well. The disadvantage is that
-- we cannot use functions defined over Int directly over Age

--data Age = Years Int

-- It is also possible to use the same name for the type and constructor, but
-- it is wise not to do so
--data Person = Person Name Age

-- An example

data Shape = Circle Float |
             Rectangle Float Float |
             Triangle Float Float
  deriving (Eq,Ord,Show,Read) -- Enum can only be used in enumerated types such as Season

-- We can also define our own instances and not use the derived methods

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound _ = False

area (Circle r) = pi*r*r
area (Rectangle h w) = h*w
area (Triangle b h) = 0.5*b*h

perimeter (Circle r) = 2*pi*r
perimeter (Rectangle h w) = 2*h+2*w
perimeter (Triangle b h) = 0 -- Meh, too lazy

-- The types can be recursive, that is, the typename (i.e. Shape) can be used in the type
-- being defined in the arguments (i.e. Float, etc). This gives way to lists, trees and other
-- data structures

-- The type name can also be followed by one or more of the type variables used on the right
-- side, making the definition polymorphic

data Month = January|February|March|April|May|June|July|August|September|October|November|December
  deriving (Eq,Ord,Enum,Show,Read)

-- Recursive algebraic types

-- An integer is either a literal expression, like 25, or is given by combining two expressions
-- using an arithmetic operator like +/-

data Expr = Lit Int | Add Expr Expr | Sub Expr Expr

-- A binary tree is either nil or is given by combining a value and two subtrees

--data NTree = NilT | Node Int NTree NTree

-- Evaluation of expressions

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2 

instance Show Expr where
  show (Lit n) = show n
  show (Add e1 e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
  show (Sub e1 e2) = "(" ++ show e1 ++ "-" ++ show e2 ++ ")"

-- Integer binary trees

data NTree = NilT | NNode Int NTree NTree

sumTree,depth :: NTree -> Int

sumTree NilT = 0
sumTree (NNode n t1 t2) = n + sumTree t1 + sumTree t2

depth NilT = 0
depth (NNode n t1 t2) = 1 + max (depth t1) (depth t2)

occurs :: NTree -> Int -> Int
occurs NilT p = 0
occurs (NNode n t1 t2) p
  | n==p       = 1 + occurs t1 p + occurs t2 p
  | otherwise  = occurs t1 p + occurs t2 p

collapse,sort :: NTree -> [Int]

collapse NilT = []
collapse (NNode n t1 t2) = collapse t1 ++ [n] ++ collapse t2

qSort [] = []
qSort (x:xs) = qSort [y | y<-xs,y<=x] ++ [x] ++ qSort [y | y<-xs,y>x]

sort = collapse .> qSort 

-- To define infix operators in a data definition, surround them in :

--data Exprr = Lit Int | Exprr :+: Exprr | Exprr :-: Exprr

-- Mutual recursion
-- A description of a person might include biographical details, which in turn refer
-- to another person
{-
data Person = Adult Name Address Biog |
              Child Name
data Biog = Parent String [Person] |
            NonParent String
-}
{-
data Espresso = Lit Int | Op Ops Espresso Espresso | If BEsp Espresso Espresso
data BEsp = BoolLit Bool |
            And BEsp BEsp |
            Not BEsp |
            Equal BEsp BEsp |
            Greater BEsp BEsp
data Ops = Add | Sub | Mult | Div
-}
-- The following two functions can be defined by mutual recursion...

--eval :: Espresso -> Int
--bEval :: BEsp -> Bool

-- Polymorphic algebraic types, the type variables appear on the left hand side of the
-- definition as well, now

data Pairs a = Pr a a

equalPair :: Eq a => Pairs a -> Bool
equalPair (Pr x y) = x==y

data List a = NilList | Cons a (List a)
  deriving (Eq,Ord,Show,Read)

-- The types of lists forms a useful paradigm for recursive polymorphic types.
-- It is possible to define use families of functions over such types,
-- and program verification can proceed by induction over the structure of a type

-- Binary trees that carry more than integers at each node

data Tree a = Nil | Node a (Tree a) (Tree a)
  deriving (Eq,Ord,Show,Read)

-- The definitions of occurs/depth/etc remain unchanged,
-- remained slightly to prevent multiplicity

depthy :: Tree a -> Int
depthy Nil = 0
depthy (Node n t1 t2) = 1 + max (depthy t1) (depthy t2)

collapsey :: Tree a -> [a]
collapsey Nil = []
collapsey (Node x t1 t2) = collapsey t1 ++ [x] ++ collapsey t2

-- Various high-ordered functions are definable as well

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Nil = Nil
mapTree f (Node x t1 t2) = Node (f x) (mapTree f t1) (mapTree f t2)

-- The union, type Either

--data Either a b = Left a | Right b
--  deriving (Eq,Ord,Read,Show)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left x) = f x
either f g (Right y) = g y


applyLeft :: (a -> c) -> Either a b -> c
--applyLeft f (Left x) = f x
--applyLeft f (Right _) = error "ApplyLeft applied to the Right"
applyLeft f = either f (error "ApplyLeft applied to Right")

twist :: Either a b -> Either b a
twist (Left x) = (Right x)
twist (Right y) = (Left y)

-- Fun trees! A general tree...

data GTree a = Leaf a | Gnode [GTree a]
  deriving (Eq,Ord,Show,Read)

extree :: GTree Int
extree = Gnode [Leaf 5, Leaf 10, Leaf 20, Gnode [Leaf 3, Leaf 4, Leaf 6]]

-- Count leafs in gtree

leafs :: GTree a -> Int
leafs (Leaf _) = 1
leafs (Gnode t) = foldr (+) 0 (map leafs t)

-- Depth of gtree

depthG :: GTree a -> Int
depthG (Leaf _) = 1
depthG (Gnode t) = 1 + (foldr max 0 (map depthG t))

-- Sum of numeric gtree

summy :: GTree Int -> Int
summy (Leaf n) = n
summy (Gnode t) = foldr (+) 0 (map summy t)

-- Whether an element is in a GTree

exists :: Eq a => a -> GTree a -> Bool
exists x (Leaf y) = x==y
exists x (Gnode t) = or (map (exists x) t)

-- General map

mapGTree :: (a -> b) -> GTree a -> GTree b
mapGTree f (Leaf x) = Leaf (f x)
mapGTree f (Gnode t) = Gnode (map (mapGTree f) t)

-- Flatten gtree into a list

flatten :: GTree a -> [a]
flatten (Leaf n) = [n]
flatten (Gnode t) = foldr (++) [] (map flatten t)
