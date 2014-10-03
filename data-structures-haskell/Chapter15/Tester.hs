{-
  Chapter 15 :: Case Study: Huffman Codes
-}
module Tester where

-- Discussion about modules..

-- A module is a collection of definitions (types, functions, etc...),
-- which a clearly defined interface stating what the module exports to other
-- modules which use or import it.

-- Parts of a system can be built separately from each other. Suppose we want to monitor traffice
-- on a network: one module might produce statistics, which another displays them in a suitable form.
-- If we agree which statistics are to be presented (their type, etc...), that is,
-- we agree on the interface, then development of the two parts of the system can go on independently.

-- Parts of a system can be compiled separately, which is a great advantage for a system of any
-- complexity.

-- Libraries of components can be reused, by importeing the appropriate modules containing them.

-- In the definition of Haskell, there is no identification between modules and files, but
-- we adopt the convention of using one module per file and that the module Foo resides in
-- the file Foo.hs or Foo.lhs.

-- Example (in Ant.hs):
--
-- module Ant where
--
-- data Ants = ...
-- anteater x = ...

-- Note that the definitions all begin the same column as the keyword 'module'. So it is safest to
-- make this the leftmost column in the file, or first tab in a literate script (lhs).

-- Importing a module means that the VISIBLE definitions of the module being imported can
-- be used in the module importing it.

-- module Bee where
-- import Ant

-- module Cow where
-- import Bee

-- But the definitions in And will not be visible in Cow

-- Also, each system of modules should contain a top-level module named Main, which gives a
-- definition of the name main. In a compiled system, this is the expression that is
-- evaluated when the compiled code is executed. In an interpreter like hugs, this is
-- of less significance. Note that a module with no explicit name is treated as Main
-- in the interpreter.

-- For exporting, we can control which definitions in the module are to be exported, as in

-- module Bee (beeKeeper, Ants(..), anteater) where ...
-- module Bee (beeKeeper, module Ant) where ...
-- (or)
-- module Bee (module Bee, module Ant) where ...

-- Thus the following two statements are equivant (if Fish imports no other modules)
-- module Fish where
-- module Fish (module Fish) where

-- We can also limit what we want to import, that is, if we only want to import type Ants then...
-- import Ant (Ants(..))

-- and if we don't want to import a name then we use the keyword hiding:
-- import Ant hiding (anteater)

-- Suppose we want to use a defintion bear from Ant, and we want to qualify it, as in,
-- use the term Ant.bear, not just bear. We should use the qualified keyword
-- import qualified Ant

-- And we can use local names for an imported module:
-- import Insect as Ant (where Insect is the name we use in the module importing Ant)

-- Prelude is implicitly imported into every module, we just have to explicitly
-- import it with the use of hiding if we wish not to import certain prelude functions

-- Or we can utilize import qualified Prelude to force the Prelude prefix on each
-- prelude definition we wish to use in the current module

-----------------------------------------------------------

-- Modular design!!!!

-- Computer systems should be designed with CHANGE in mind, so they should be documented
-- thoroughly. There are a few key points to keep in mind when designing a modular system.

-- 1. Each module should have a clearly defined role

-- 2. Each module should do one thing only. If a module has two separate purposes, these
-- should be split into two separate modules. The chance of a change to one affecting the
-- other is thereby reduced

-- 3. Each part of the system should be performed by a single module: each module should
-- do one thing completely; it should be self-contained, in other words. If performing
-- one part of the whole is split into two modules, then either their should should be
-- merged, or there should be a module defined with a single purpose of bringing the
-- two components together

-- 4. Each module should export only what is necessary. It is then clearer what the effect
-- of an import is: precisely the functions which are needed are imported. In other
-- words, information hiding

-- 5. Modules should be small. It is good practice that no module should be larger than can
-- be printed on two or three sides of paper.

-- Also keep in mind design for reuse, particularly in the context of polymorphic types
-- and higher-order functions. The module is the unit of reuse, and a library will be
-- accessed by means of an import statement.

-- On including a general-purpose module, it is possible to suppress the definitions
-- which are not used.

-- A qualified import can be used to avoid the name-clashes which can often occur,
-- despite the infinite choice of names for functions, in practice we tend to choose from
-- a very small subset.

--------------------------------------------------------

-- The exercise in this chapter will be based on encoding/decoding messages
-- using a binary tree where sequences of L | R are used to traverse the branches
-- to the leaf nodes where the next char in the sequence is stored as a value.

-- Not too difficult to see with pictures, too bad I'm not going going to go
-- through the trouble of illustrating a tree in VI, here... just start with Types.hs
-- and you'll figure out what the heck's goin on, I hope!














