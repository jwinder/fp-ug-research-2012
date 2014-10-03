-----------------------------------------------------------------------
-- 	Index.hs
-- 	
-- 	(c) Simon Thompson, 2001
-- 
-- 	A simple (line) indexing program.
-----------------------------------------------------------------------

module Index where
{- helium changes
introduce explicit definition of elem over Char
replace == and < on String by eqString and ordString (followed by pattern match)
replace /= on String by not.eqString
-}

-- Function composition and forward composition
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- A composition operator taking its arguments in the opposite order to `.'.
-- The definition is preceded by a fixity declaration for the operator.

infixl 9 >.>

(>.>) :: (a -> b) -> (b -> c) -> (a -> c)

g >.> f = f . g


-- Creating an index
-- ^^^^^^^^^^^^^^^^^

-- The basic type symonyms

type Doc  = String
type Line = String
type Word = String

-- Example document

exDoc1, exDoc2 :: Doc

exDoc1 = "cathedral doggerel cathedral\nbattery doggerel cathedral\ncathedral"
exDoc2 = "cathedral doggerel Cathedral\nbattery Doggerel cathedral\ncathedral"

-- The type of the top-level function

makeIndex :: Doc -> [ ([Int],Word) ]

-- The top-level definition

makeIndex
  = lines       >.>     --   Doc            -> [Line]
    numLines    >.>     --   [Line]         -> [(Int,Line)] 
    allNumWords >.>     --   [(Int,Line)]   -> [(Int,Word)]
    sortLs      >.>     --   [(Int,Word)]   -> [(Int,Word)]
    makeLists   >.>     --   [(Int,Word)]   -> [([Int],Word)]
    amalgamate  >.>     --   [([Int],Word)] -> [([Int],Word)]
    shorten             --   [([Int],Word)] -> [([Int],Word)]

-- Implementing the component functions
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
 
-- Attach a number to each line.

numLines :: [Line] -> [ ( Int , Line ) ]
numLines linels
  = zip [1 .. length linels] linels

-- Associate each word with a line number

numWords :: ( Int , Line ) -> [ ( Int , Word ) ]

numWords (number , line)
  = [ (number , word) | word <- splitWords line ]

-- The definition uses functions defined in Chaoter 7 of the notes/book, with
-- a modified version of whitespace. The definitions are at the end of this
-- file.

-- Apply numWords to each integer,line pair.

allNumWords :: [ ( Int , Line ) ] -> [ ( Int , Word ) ]
allNumWords = concat . map numWords

-- The list must next be
-- sorted by word order, and lists of lines on which a word appears be built.
-- The ordering relation on pairs of numbers and 
-- words is given by

orderPair :: ( Int , Word ) -> ( Int , Word ) -> Bool
orderPair ( n1 , w1 ) ( n2 , w2 )
  = lessString w1 w2 || ( eqString w1 w2 && n1 < n2 )

lessString st1 st2 = isLess (ordString st1 st2)

isLess LT = True
isLess _  = False

-- Sorting the list using the orderPair ordering on pairs.

sortLs :: [ ( Int , Word ) ] -> [ ( Int , Word ) ]

sortLs []     = []
sortLs (p:ps)
  = sortLs smaller ++ [p] ++ sortLs larger
    where
    smaller = [ q | q<-ps , orderPair q p ]
    larger  = [ q | q<-ps , orderPair p q ]

-- The entries for the same word need to be accumulated together.
-- First each entry is converted to having a list of line numbers associated with
-- it, thus

makeLists ::  [ (Int,Word) ] -> [ ([Int],Word) ]
makeLists 
  = map mklis 
    where
    mklis ( n , st ) = ( [n] , st )

-- After this, the lists associated with the same words are amalgamated.

amalgamate :: [ ([Int],Word) ] -> [ ([Int],Word) ]

amalgamate [] = []
amalgamate [p] = [p]
amalgamate ((l1,w1):(l2,w2):rest)
  | not (eqString w1 w2)    = (l1,w1) : amalgamate ((l2,w2):rest)
  | otherwise   = amalgamate ((l1++l2,w1):rest)

-- Remove all the short words.

shorten :: [([Int],Word)] -> [([Int],Word)]

shorten inx = [ (nl,wd) | (nl,wd)<-inx, length wd > 3 ]


-- Splitting a string into words, removing punctuation.
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

elem :: Char -> [Char] -> Bool

elem c st = or [ eqChar c ch | ch <- st ]

-- Taken from notes/book, Chapter 7.

whitespace :: String
whitespace = " \n\t;:.,\'\"!?()-"

-- Get a word from the front of a string.

getWord :: String -> String
getWord []    = []
getWord (x:xs)
  | elem x whitespace	= []
  | otherwise		= x : getWord xs

-- In a similar way, the first word of a string can be dropped.

dropWord :: String -> String
dropWord []    = []
dropWord (x:xs)
  | elem x whitespace	= (x:xs)
  | otherwise		= dropWord xs

-- To remove the whitespace character(s) from the front of a string.

dropSpace :: String -> String
dropSpace []	= []
dropSpace (x:xs)
  | elem x whitespace	= dropSpace xs
  | otherwise		= (x:xs)

-- Splitting a string into words.

splitWords :: String -> [Word]
splitWords st = split (dropSpace st)

split :: String -> [Word]
split [] = []
split st
  = (getWord st) : split (dropSpace (dropWord st))


