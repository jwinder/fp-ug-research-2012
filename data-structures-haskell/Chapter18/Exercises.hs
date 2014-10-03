-- Joseph Winder

module Exercises where

-------
-- 18.1
-------

-- Write a program which reads a line of input and tests whether it is a
-- palindrome, also displaying appropriate messages.

isPalindrome :: IO ()
isPalindrome
  = do putStr "Enter a string: "
       line <- getLine
       putStr line
       if line == reverse line
         then putStrLn " is a palindrome."
         else putStrLn " is not a palindrome."

-------
-- 18.2
-------

-- Write a program which reads two integers and returns their sum.

findSum :: IO ()
findSum
  = do putStr "Enter the first number: "
       num1 <- getLine
       putStr "Enter the second number: "
       num2 <- getLine
       putStr "The Sum is: "
       putStrLn (show ((read num1 :: Int) + (read num2 :: Int)))

-------
-- 18.4
-------

-- Write a program which reads lines and tests whether they are palindromes
-- until an empty line is read.

isPal :: IO ()
isPal
  = do line <- getLine
       if line == []
          then return ()
          else (do if line == reverse line
                      then putStrLn "Palindrome!\n"
                      else putStrLn "Not a palindrome...\n"
                   isPal)

palindromes :: IO ()
palindromes
  = do putStrLn "\nEnter strings (separated by lines) and the program will test whether they are palindromes."
       putStrLn "Enter an empty string to exit.\n"
       isPal


--------
-- 18.22
--------

-- Show that sets and binary trees can be given a monadic structure, as can the type:
-- data Error a = OK a | Error String

instance Monad Error where
  (OK x) >>= f     = f x
  (Error s) >>= f  = Error s
  return           = OK
  fail             = Error

-- data Tree a = Nil | Node a (Tree a) (Tree a)

instance Monad Tree where
  Nil >>= f             = Nil
  (Node x t1 t2) >>= f  = (f x) t1 t2
  return x              = Node x Nil Nil
  fail s                = Nil

--------
-- 18.24
--------

-- Define an alternative monadic structure over lists.

-- The given definition is:

instance Monad [] where
  xs >>= f  = concat (map f xs)
  return x  = [x]
  fail s    = []

instance Monad [] where
  [] >>= f    = []
  (x:xs) >>=  = (f x):(xs >>= f)
  return x    = x:[]
  fail s      = []
