module BBTree (BBTree (Nil,Node), insert, exists, size, emptyTree, toBBTree, toList) where

data BBTree a = Nil | Node a (BBTree a) (BBTree a)
  deriving (Eq,Ord,Show)

------------------------------------------
insert    :: Ord a => BBTree a -> a -> BBTree a
remove    :: Ord a => BBTree a -> a -> BBTree a
exists    :: Ord a => BBTree a -> a -> Bool
size      :: Ord a => BBTree a -> Int
depth     :: Ord a => BBTree a -> Int
empty     :: Ord a => BBTree a -> Bool
emptyTree :: Ord a => BBTree a
toBBTree  :: Ord a => [a] -> BBTree a
toList    :: Ord a => BBTree a -> [a]
------------------------------------------

insert Nil x = Node x Nil Nil
insert (Node x t1 t2) y
  | y<=x       = let t = Node x (insert t1 y) t2 in balance t
  | otherwise  = let t = Node x t1 (insert t2 y) in balance t

remove t y = toBBTree (filter (/=y) $ toList t)

exists Nil _ = False
exists (Node x t1 t2) y = y==x || if y<=x then exists t1 y else exists t2 y

size Nil = 0
size (Node _ t1 t2) = 1 + size t1 + size t2

depth Nil = 0
depth (Node _ t1 t2) = 1 + max (depth t1) (depth t2)

empty Nil = True
empty _ = False

emptyTree = Nil

toBBTree [] = Nil
toBBTree (x:xs) = insert (toBBTree xs) x

toList Nil = []
toList (Node x t1 t2) = toList t1 ++ [x] ++ toList t2

-- Private helper functions that balance the tree.

balance :: Ord a => BBTree a -> BBTree a
balance Nil = Nil
balance (Node x t1 t2)
  | depth t1 > depth t2  = leftHeavyBalance (Node x t1 t2)
  | depth t1 < depth t2  = rightHeavyBalance (Node x t1 t2)
  | otherwise            = Node x (balance t1) (balance t2)

leftHeavyBalance :: Ord a => BBTree a -> BBTree a
leftHeavyBalance (Node x (Node y s1 Nil) t2) = Node y (balance s1) (balance $ Node x Nil t2)
leftHeavyBalance (Node x (Node y s1 (Node z u1 u2)) t2)
  | depth s1 > depth (Node z u1 u2)  = Node y (balance s1) (balance $ Node x (Node z u1 u2) t2) -- Single right rotation.
  | otherwise                        = Node z (balance $ Node y s1 u1) (balance $ Node x u2 t2) -- Left-Right rotation.

rightHeavyBalance :: Ord a => BBTree a -> BBTree a
rightHeavyBalance (Node x t1 (Node y Nil s2)) = Node y (balance $ Node x t1 Nil) (balance s2)
rightHeavyBalance (Node x t1 (Node y (Node z u1 u2) s2))
  | depth (Node z u1 u2)  < depth s2  = Node y (balance $ Node x t1 (Node z u1 u2)) (balance s2) -- Single left rotation.
  | otherwise                         = Node z (balance $ Node x t1 u1) (balance $ Node y u2 s2) -- Right-left rotation.
