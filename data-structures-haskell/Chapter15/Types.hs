-- Types.hs
-- Types used in the Huffman coding example! Page 287 of Simon Thompson's Haskell

module Types (Tree(Leaf,Node),
              Bit(L,R),
              HCode,
              Table) where

-- Trees to represent the relative frequences of chars and therefore Huffman codes

data Tree = Leaf Char Int |
            Node Int Tree Tree

-- The types of bits, Huffman codes and tables of Huffman codes

data Bit = L | R deriving (Eq,Show)

type HCode = [Bit]

type Table = [ (Char,HCode) ] 
