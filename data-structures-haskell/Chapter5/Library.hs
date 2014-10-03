module Library where

import Prelude
import Char

-- Types we will need. It will be assumed that the database consists of a Person/Book pair
-- to promote a symmetric relationship

type Person = String
type Book = String
type Database = [(Person,Book)]

-- Example data

exampleBase :: Database
exampleBase = [("Alice","Tintin"), ("Anna","Little Women"), ("Alice","Asterix"), ("Rory","Tintin")]

-- Accessor type functions we need 

--books :: Database -> Person -> [Book]
books db person = [book | (per,book) <- db, per == person]

--borrowers :: Database -> Book -> [Person]
borrowers db book = [per | (per,b) <- db, b == book]

--borrowed :: Database -> Book -> Bool
borrowed db book = length (borrowers db book) > 0

--numBorrowed :: Database -> Person -> Integer
numBorrowed db person = length (books db person)

-- Update functions

--makeLoan :: Database -> Person -> Book -> Database
--returnLoad :: Database -> Person -> Book -> Database

makeLoan db per bk = [(per,bk)] ++ db

returnLoan db per bk = [pair | pair <- db, pair /= (per,bk)]
