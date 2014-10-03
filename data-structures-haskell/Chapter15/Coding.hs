-- Coding.hs

module Coding (codeMessage, decodeMessage) where

import Types (Tree(Leaf,Node), Bit(L,R), HCode, Table)

codeMessage :: Table -> [Char] -> HCode
codeMessage tbl = concat . map (lookupTable tbl)

lookupTable :: Table -> Char -> HCode
lookupTable [] c = error "lookupTable"
lookupTable ((ch,n):tb) c
  | ch==c      = n
  | otherwise  = lookupTable tb c

-- Note that because of the module statement, this module is not exported.

decodeMessage :: Tree -> HCode -> [Char]
decodeMessage tr = decodeByT tr
  where
    decodeByT (Node n t1 t2) (L:rest) = decodeByT t1 rest
    decodeByT (Node n t1 t2) (R:rest) = decodeByT t2 rest
    decodeByT (Leaf c n) rest = c : decodeByT tr rest
    decodeByT t [] = []

exam1 = Node 0 (Leaf 'a' 0)
               (Node 0 (Leaf 'b' 0) (Leaf 't' 0))

mess1 = [R,L,L,R,R,R,R,L,R,R]

table1 = [ ('a',[L]), ('b',[R,L]), ('t',[R,R]) ]
