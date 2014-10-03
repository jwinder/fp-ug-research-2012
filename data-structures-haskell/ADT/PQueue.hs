module PQueue where

-- In this implementation, 'p' will be the priority and 'a' the value.
newtype Ord p => PQueue a p = PQueue [(a,p)]
  deriving (Show)

------------------------------------------
insert        :: Ord p => PQueue a p -> a -> p -> PQueue a p
insertHighest :: Ord p => PQueue a p -> a -> Maybe (PQueue a p)
insertLowest  :: Ord p => PQueue a p -> a -> Maybe (PQueue a p)
removeHighest :: Ord p => PQueue a p -> Maybe (a, PQueue a p)
removeLowest  :: Ord p => PQueue a p -> Maybe (a, PQueue a p)
highestElem   :: Ord p => PQueue a p -> Maybe a
lowestElem    :: Ord p => PQueue a p -> Maybe a
highestPrior  :: Ord p => PQueue a p -> Maybe p
lowestPrior   :: Ord p => PQueue a p -> Maybe p
isEmpty       :: Ord p => PQueue a p -> Bool
emptyPQueue   :: Ord p => PQueue a p
size          :: Ord p => PQueue a p -> Int
toPQueue      :: Ord p => [a] -> p -> PQueue a p
------------------------------------------

insert (PQueue xs) x prior = PQueue ( [(a,ap) | (a,ap)<-xs, ap>prior] ++ [(x,prior)] ++ [(b,bp) | (b,bp)<-xs, bp<=prior] )

insertHighest (PQueue []) _ = Nothing
insertHighest (PQueue xs) x = Just (insert (PQueue xs) x (snd $ head xs))

insertLowest (PQueue []) _ = Nothing
insertLowest (PQueue xs) x = Just (insert (PQueue xs) x (snd $ last xs))

removeHighest (PQueue []) = Nothing
removeHighest (PQueue xs) = Just (fst $ head xs, PQueue $ tail xs)

removeLowest (PQueue []) = Nothing
removeLowest (PQueue xs) = Just (fst $ last xs, PQueue $ init xs)

highestElem (PQueue []) = Nothing
highestElem (PQueue xs) = Just (fst $ head xs)

lowestElem (PQueue []) = Nothing
lowestElem (PQueue xs) = Just (fst $ last xs)

highestPrior (PQueue []) = Nothing
highestPrior (PQueue xs) = Just (snd $ head xs)

lowestPrior (PQueue []) = Nothing
lowestPrior (PQueue xs) = Just (snd $ last xs)

isEmpty (PQueue xs) = null xs

emptyPQueue = PQueue []

size (PQueue xs) = length xs

toPQueue xs prior = PQueue [(x,prior) | x<-xs]
