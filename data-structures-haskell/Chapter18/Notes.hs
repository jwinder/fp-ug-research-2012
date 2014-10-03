-- Chapter 17 :: Programming With Actions
-- Joe Winder
module Notes where
import IO

-- This chapter explores developing programs, as well as reading/writing to the terminal.
-- The input/output solution in Haskell is to introduce the types IO a, which are programs
-- that do some input/output before returning a value of type a.

-- The sequential nature of IO a types are not particular to input/output. They are a simple
-- example of a more general concept known as a monad. Other examples are side-effects,
-- error-handling and non-determinacy.

-- Monads provide a powerful structuring mechanism for function programs incorporating
-- their side-effects, as well as providing an interface between the functional and
-- imperative worlds. We illustrate this versatility by showing that two substantially
-- different programs over a tree will have the same top-level structure if they are
-- programmed in a monadic style.

-----------------------------
-- Why is I/O an issue?
-----------------------------

-- A functional program consists of a number of definitions, such as

val :: Int
val = 42

function :: Int -> Int
function n = val+n

-- The effect of these definitions is to associate a fixed value with each name; in the
-- case of val, the value is an integer and in the case of function, it is a function
-- from the integers to the integers. How is an input or an output to fit into this model?

-- One approach is to include operations like:

--inputInt :: Int

-- whose effect is to read an integer from an input; the value read in becomes the value
-- given to inputInt. Each time inputInt is evaluated it will be given a new value, and so
-- it is not a fixed integer value as it ought to be according to our original model.

-- Consider the following:

--inputDiff = inputInt - inputInt

-- This function breaks the model of reasoning which is used. It would expect subtracting
-- a value from itself and given a value of zero. But that might not be the case when
-- both inputInts might be different if the item inputs are different in each case. So we
-- cannot determine inputDiff by looking at the meaning of its individual thoughts without
-- knowing 'where' they occur in the program.

-- The basic idea of monadic I/O is to control how programs that perform I/O are built,
-- and, in particular, to limit the way that I/O operations affect functions in general.

--------------------------
-- The basics of I/O
--------------------------

-- In thinking about I/O, it helps to think of actions happening in a sequence. For ex, first
-- some input might read, and then on the basis of that some further input might be read,
-- or output might be produced.

-- Haskell provides the types IO a of I/O actions of type a (or I/O programs of type a). An
-- object belonging to IO a is a program which will do some I/O and return a value of type a.

-- One way of looking at IO a types is that they provide a simple imperative programming
-- language for writing I/O programs on top of Haskell, without compromising the functional model of
-- Haskell itself.

-- An example of Reading Input:

-- The operation which reads a line of text from the standard input does some I/O and
-- returns a string which is the line just read. According to the explanation above, this
-- should be an object of type IO String. So we have the standard functions:

--getLine :: IO String
--getChar :: IO Char

-- Haskell also contains the type (), which contains one element only. This element is also
-- written as (). A value of this type can convey no useful information and so the type is not
-- often used. However, it is useful in programming IO, as there are cases of IO programs
-- whose only significance is their I/O actions and not the results they return. So we also
-- have the type IO (), and they will return () as their result.

-- An example of writing Strings:

-- The operation of writing the string Hello World will be an object which performs
-- some I/O but which has nothing of significance to pass back to the program. It is
-- therefore of type IO (). The general operation to print a text string will be a function
-- which takes the string and gives back the I/O object which writes that string.

--putStr :: String -> IO ()

helloWorld :: IO ()
helloWorld = putStr "Hello World!"


-- And we have a function to write an entire line of input:

--putStrLn :: String -> IO ()
--putStrLn = putStr . (++ "\n")

-- This function just adds a newline to the ind of the input before passing it to putStr.

-- About writing values in general:

-- The Haskell prelude provides the class Show with the function

--show :: Show a => a -> String

-- which can be used to write values of many types. For example, we can define a general
-- print function from the standard prelud thus

--print :: Show a => a -> IO ()
--print = putStrLn . show

-- Returning a value: the keyword 'return'

-- Suppose we want to write an I/O action which does no I/O but does return a value - we
-- will see examples of this in the future. This is achieved by the built-in function

--return :: a -> IO a

-- The effect of Io x is to do no I/O, but simply to return the result x.

-- Running an I/O program:

-- We have written a simple I/O program, namely helloWorld; how is it run? In Hugs,
-- we can evaluate it at the prompt. Strictly speaking, the main function of a Haskell
-- program should be of type IO a, for some a. In Hugs, if we ask to evaluate an expression
-- e of type b, then it is wrapped up as an object of type IO () by applying
-- the print function.

------------------------
-- The do notation
------------------------

-- The do notation is useful to sequence I/O programs and 'capture' the values
-- returned by IO actions and so to pass these values to actions which follow
-- them in a program.

-- Example 1:

--putStrLn str = do putStr str
--                  putStr "\n"

-- This example puts the string str to output, and then the newline. The do syntax
-- is governed by the offside rule.

-- Example 2:

put4times :: String -> IO ()
put4times str = do putStrLn str
                   putStrLn str
                   putStrLn str
                   putStrLn str

-- Example 3:

putNtimes :: Int -> String -> IO ()
putNtimes n str
  = if n<=1
    then putStrLn str
    else do putStrLn str
            putNtimes (n-1) str

-- And we can redefine example 2: put4times = putNtimes 4

-- Example 4:

-- We can also make inputs of a part of a sequence of actions. I.e. we can 
-- read two lines of input and then output the message:

read2lines :: IO ()
read2lines = do getLine
                getLine
                putStrLn "Two lines read..."

-- And it would be easy to write readNlines.

-- Example 5:

-- The last example read two lines but did nothing with the input, we can name
-- the results of IO a actions. A program to read a line and then write that line
-- is given by

getNput :: IO ()
getNput = do line <- getLine
             putStrLn line

-- where line <- getLine names the result of getLine, it can be thought of as
-- an assignment 'line := getLine' in an imperative style. But there are a lot
-- of differences in the schools of thought. 'var <-' creates a new variable
-- and so the language permits 'single assignment' rather than the
-- updatable assignment familar in most imperative languages.

-- Example 6:

reverse2lines :: IO ()
reverse2lines
  = do line1 <- getLine
       line2 <- getLine
       putStrLn (reverse line2)
       putStrLn (reverse line1)

-- Example 7:

-- The previous example can be redefined to contain local definitions of the
-- reversed lines

reverse2Lines :: IO ()
reverse2Lines
  = do line1 <- getLine
       line2 <- getLine
       let rev1 = reverse line1
       let rev2 = reverse line2
       putStrLn rev2
       putStrLn rev1

-- Recall that Haskell contains the class Read:

--read :: Read a => String ->

-- which can parse a string to a particular type.

-- Example 8:

getInt :: IO Int
getInt = do line <- getLine
            return (read line :: Int)

--------------------------------
-- Iteration and Recursion
--------------------------------

-- This section discusses building I/O programs using repetition, involving
-- a general while-loop.

-- A while loop:

-- Suppose that we want to repeat an IO () action while a condition is true.
-- The condition will depend upon the I/O system, and so will be of type IO Bool.

-- An example of this, which is provided in the library module IO.hs, is a test
-- for the end of input,

--isEOF :: IO Bool

-- So our while loop construct will have the type:

while :: IO Bool -> IO () -> IO ()

-- And is defined by:

while test action
  = do res <- test
       if res then do action
                      while test action
              else return ()

-- An example: Copying input to output, line by line, using a while loop:

copyInputToOutput :: IO ()
copyInputToOutput
  = while (do res <- isEOF
              return (not res))
          (do line <- getLine
              putStrLn line)

-- Note that the parenthesis above are necessary

goUntilEmpty :: IO ()
goUntilEmpty
  = do line <- getLine
       if line == []
          then return ()
          else (do putStrLn line
                   goUntilEmpty)

-- Adding a sequence of integers!

sumInts :: IO Int
sumInts
  = do n <- getInt
       if n==0
          then return 0
          else (do m <- sumInts
                   return (n+m))

-- It is interesting to compare this with the recursion in:

sumList [] = 0
sumList (n:ns) = n + sum ns

-- Or with a modified definition of sum:

summy [] = 0
summy (n:ns) = let m = sum ns in (n+m)

-- And now, a 'wrapper' program that displays prompts for sumInts:

sumInteract :: IO ()
sumInteract
  = do putStrLn "Enter integers, one per line. These will be summed until zero is entered."
       sum <- sumInts
       putStr "The sum is "
       print sum

------------------------------------
-- Monads for functional programming
------------------------------------

-- The monadic style extends beyond I/O to cover a number of fields.

-- A characteristic of monads is that they make explicit the sequence in which
-- operations take place. It is the do construct, which is itself based on
-- the combinator (>>=) which sequences the operations of a general monad m.

(>>=) :: m a -> (a -> m b) -> m b

-- When would this sequence be necessary? Consider the following expression:
-- e - f
-- As this is simply an expression, we can choose to evaluate the arguments to
-- -, e, and f, in either order, or in parallel. However, suppose that the expressions
-- e and f cause some I/O to take place or cause some store to be changed. Then we
-- need to say in which order the evaluation takes place, since different orders
-- will give different events.

-- This problem was expressed earlier, as inputInt - inputInt would be
-- different depending on which event takes place first. To achieve an explicit sequence
-- we would use this:
-- do e <- getInt
--    f <- getInt
--   return (e-f)

-- This sort of explicit sequencing is a feature of many kinds of programming where
-- side-effects accompany a computation. The novel feature of the monadic approach
-- is that these side-effects can be incorporated into a pure functional programming
-- language by means of monads.

-- What is a monad?

-- A monad is a family of types m a, based on a polymorphic type constructor m, with
-- functions return, (>>=), (>>), and fail

class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  return :: a -> m a
  (>>)   :: m a -> m b -> m b
  fail   :: String -> m a

-- This is an example of a constructor class, which is similar to a type class except
-- that the things which belong to a constructor class are type constructors, that is,
-- functions which build types from types -- rather than just types.
-- Examples of type constructors are 'list', written [] in Haskell, and IO.

-- The definition of Monad also contains default declarations for >> and fail:

m >> k = m >>= \_ -> k
fail s = error s

-- The (>>) obviously works just like (>>=) except that the first argument of the function of
-- type (a -> m b) is discarded rather than being passed to the second argument.

-- In order properly to be a monad, the functions return and (>>=) and the value zero should
-- have some simple properties. Informally, the requirements are:

-- 1. The operation return x should simply retur the value x, without any addition computational
--    effect, such as input or output in the case of IO monad.
-- 2. The sequencing given by (>>=) should be irrelevant of the way that expressions are bracketed.
-- 3. The value fail s corresponds to the computation which fails, giving the error message s.

-- The laws are much clearer when stated in terms of a derived operator, such as >@>.

(>@>) :: Monad m => (a -> m b) ->
                    (b -> m c) ->
                    (a -> m c)

f >@> g = \x -> (f x) >>= g

-- This operator generalizes function composition (Kleiskli composition in category theory) in that
-- it composes objects (a -> m b) and (b -> m c) to give (a -> m c). -- The picture in the book
-- does this more justice.

-- Note that the return is of similar shape, as it is (a -> m a).

-- Now we state formally the rules that the operations of a monad should satisfy. First, return is an identity:

return >@> f = f
f >@> return = f

--  and the operator should be associative:

(f >@> g) >@> h = f >@> (g >@> h)

-- The derived operator (>>) is then also associative.


-- The rules can also be stated in terms of do notation:

m >>= f = do x <- m
             f x

-- The first two rules become:

do y <- return x = f x
   f y
do x <- m        = m
   return x

-- Some Examples of Monads!

-- As a review, a monad m a can be throught of as representing some sort of computation, with elements
-- of m being 'computations' which perform actions of some sort before returning a value of type a.

-- The identity monad:
----------------------

-- The identity monad, which takes a type to itself, is the simplest example of a monad,

m >>= f = f m
return = id

-- Under this interpretation, >@> becomes a forward composition of functions, >.> which is associative
-- and has id as its identity. An undefined computation sequenced with any other computation will be
-- undefined.
-- Computationally, this monad represents the trivial state in which no actions are performed, and values
-- are returned immediately.

-- The input/output monad:
--------------------------

-- This was already covered in the beginning of the chapter. Where the I/O subset of monad a was given by the
-- type IO a, and a basic do-notation being:

-- do str <- getLine
--    putStrLn(str)

-- where the (>>=) would look something like this:

-- getLine >>= putStrLn
-- (I think, this part isn't outlined in the book).

-- The list monad:
------------------

instance Monad [] where
  xs >>= f  = concat (map f xs)
  return x  = [x]
  fail s    = []

-- The computational interpretation of the list monad is of non-deterministic computation: an element
-- of [a] represents ALL the results of a potentially non-deterministic computation.
-- In this case the return gives a single answer while (>>=) applies the function f to every possible
-- outcome in xs, and concatenates the results to give a single list of overall outcomes. The value fail s
-- corresponds to there being no result of the non-deterministic computation; a failure to give a result,
-- in other words.

-- The maybe monad:
-------------------

instance Monad Maybe where
  (Just x) >>= k  = k x
  Nothing >>= k   = Nothing
  return          = Just
  fail s          = Nothing


-- The computational interpretation here is of computations which might produce a result, but that might also
-- produce an error.

-- The state monad:
-------------------

-- Later in this chapter, we will give an example of a state monad, State a b. An operation of this type
-- can change the state (of type a) before returning a value of type b.

-- Combining monads: The monads here can be combined to give more complex effects, so that one can
-- build computations which perform I/O and manipulate a state value, for instance.

-- Some standard functions:

-- We can define some standard functions over every monad:

mapF :: Monad m => (a -> b) -> m a -> m b
joinM :: Monad m => m (m a) -> m a


mapF f m = do x <- m
           return f m

joinM m = do x <- m
          x

-- Over lists these functions are map and concat; many of the properties of map and concat over lists lift
-- to these functions. For instance, we can show using the three monad properties that:
--mapF (f.g) = mapF f . mapF g

---------------------------------------------
-- An Example: Monadic computation over trees
---------------------------------------------

-- We now illustrate how computations over the type of
-- data Tree a = Nil | Node a (Tree a) (Tree b)
-- can be given a monadic structure.

-- Summing a tree of integers

sTree :: Tree Int -> Int
sTree Nil = 0
sTree (Node n t1 t2) = n + sTree t1 + sTree t2

-- How might a monadic solution proceed?

sumTree :: Tree Int -> St Int

-- where St is a monad which is not defined yet.

sumTree Nil = return 0
sumTree (Node n t1 t2)
  = do num <- return n
       s1  <- sumTree t1
       s2  <- sumTree t2
       return (num+s1+s2)

-- The operations are in a do-notation sequence. First we get the value of n, then
-- we calculate sumTree t1 and sumTree t2. Then we return the sum of the three values.

-- Since we are only calculating values and not trying to do any I/O or other side-effect,
-- we make the monad St the identity monad Id which we mentioned earlier.

data Id a = Id a

instance Monad Id where
  return          = Id
  (>>=) (Id x) f  = f x

-- So we can say:

sumTree :: Tree Int -> Id Int

-- Then, to give a function of type Tree Int -> Int we compose with an extract function:

--extract . sumTree

-- where

extract :: Id a -> a
extract (Id x) = x

-- Using a state monad in a tree calculation
--------------------------------------------

-- We want to write a function

numTree :: Eq a => Tree a -> Tree Int

-- So that given an arbitrary tree we transofrm it to a tree of integers in which the original
-- elements are replaced by natural numbers, starting at zero. The same element has to be
-- replaced by the same number at every occurence, and when we meet an as-yet-unvisited element,
-- we have to find a new number to match it with.

numberTree :: Eq a => Tree a -> State a (Tree Int)

-- is another possibility, where the monad State a will carry about enough information to allow
-- us to replace the elements in the correct way. The structure of the program then is

numberTree Nil = return Nil
numberTree (Node x t1 t2)
  = do num <- numberNode x
       nt1 <- numberTree t1
       nt2 <- numberTree t2
       return (Node num nt1 nt2)

-- The structure here is the same as that of sumTree. We also need to identify the monad State a
-- and to define the function which replaces an individual entry,

numberNode :: Eq a => a -> State a Int

-- We now have to think about the implementation of the monad. We have called it State since it keeps
-- a record of the state, that is, of which values are associated with which numbers. This can be done
-- in a table:

type Table a = [a]

-- where the table [True,False] indicates that True is associated with 0 and False with 1.

-- The State monad then consists of the functions

data State a b = State (Table a -> (Table a, b))

-- which, after stripping off the constructor State, we can think of as taking the state before doing
-- the operation to the state after the operation, together with its result. In other words, we
-- return a value of type b, but perhaps we change the value of the state of type Table a as a side-effect.

-- What about the monad operations?

instance Monad (State a) where
  return x = State (\table -> (tab,x))
  (State st) >>= f
    = State (\tab -> let
                     (newTab,y)    = st tab
                     (State trans) = f y
                     in
                     trans newTab)

-- In the bind, to perform st, we pass it to the table tab; the output of this is a new state, newTab,
-- as well as a value y. This y is passed to f, giving an object of type State a b; that is then performed
-- starting with the new state newTab.

-- The steps here are done in a sequence, giving a monad. Now we can define numberNode.

numberNode :: Eq a => a -> State a Int
numberNode x = State (nNode x)

nNode:: Eq a => a -> (Table a -> (Table a, Int))
nNode x table
  | elem x table  = (table,lookup x table)
  | otherwise     = (table++[x], length table)

lookup :: Eq a => a -> Table a -> Int

-- If x is an element of table, we return its position in the table, given by lookup. If it is not,
-- we add it to the end of the table and return its position, which is length table.

-- We are almost done, except that we need an extract function to return the calculation over the State a b.

extract :: State a b -> b
extract (State st) = snd (st [])

-- where st is applied to the initial state []. The result of this is a pair, from which we select
-- the second element. How we can define the original function:

numTree :: Eq a => Tree a -> Tree Int
numTree = extract . numberTree

-- To conclude, a complex calculation over a tree (numberTree) can be structured in a similar way as a simple one
-- (numTree). In the case of a tree type the advantage is tangible, but for more complex types a monadic structure
-- becomes almost essential if we are to follow a computation with complicated side-effects.
