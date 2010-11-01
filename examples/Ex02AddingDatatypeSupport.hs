
-----------------------------------------------------------------------------
-- |
-- Module      :  Ex02AddingDatatypeSupport
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
--
-- Second example module of using the EMGM library --- using the predefined
-- generic functions on user-defined datatypes.

-- This example module shows how you can use the EMGM library to apply
-- predefined generic functions to your own datatypes. Functionality such
-- as enumeration and flattening is examplified. Several terms are
-- successively defined and their evaluation is shown in a comment after the
-- definition.
--
-----------------------------------------------------------------------------

-- We need a few language extensions. The easiest way is to add the following
-- comments.
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Ex02AddingDatatypeSupport where

import qualified Generics.EMGM as G
import Generics.EMGM.Base

-- Using generic functions on your own datatypes

-- Let's define some datatypes:
data Tree a = Leaf a | Branch (Tree a) (Tree a)

-- For your datatypes, you have to define your own instances as below:
--    Embedding projection pair (conversion between the datatype and a
--    structural generic representation):
epTree = EP from' to' where
  from' (Leaf aa) = L (aa)
  from' (Branch aa ab) = R (aa :*: (ab))
  to' (L (aa)) = (Leaf aa)
  to' (R (aa :*: (ab))) = (Branch aa ab)

--    Description of the constructors:
conLeaf, conBranch :: ConDescr
conLeaf = ConDescr {
  conName = "Leaf",
  conArity = 1, conRecord = False, conFixity = Prefix
  }

conBranch = ConDescr {
  conName = "Branch",
  conArity = 2, conRecord = False, conFixity = Prefix
  }

--    More structural definition:
rTree :: Generic g => g a -> g (Tree a)
rTree ra = rtype epTree (rsum (rcon conLeaf (ra)) ((rcon conBranch (rprod (rTree ra) (rTree ra)))))

--    Instance of Rep:
instance (Generic g, Rep g a) => Rep g (Tree a) where
  rep = rTree rep

--    For container types, these should also be specified:
rTree2 :: Generic2 g => g a b -> g (Tree a) (Tree b)
rTree2 ra = rtype2 epTree epTree (rsum2 (rcon2 conLeaf ra) ((rcon2 conBranch (rprod2 (rTree2 ra) (rTree2 ra)))))

rTree3 :: Generic3 g => g a b c -> g (Tree a) (Tree b) (Tree c)
rTree3 ra = rtype3 epTree epTree epTree (rsum3 (rcon3 conLeaf ra) ((rcon3 conBranch (rprod3 (rTree3 ra) (rTree3 ra)))))

instance Generic g => FRep g Tree where
  frep = rTree

instance Generic2 g => FRep2 g Tree where
  frep2 = rTree2

instance Generic3 g => FRep3 g Tree where
  frep3 = rTree3

-- The code to allow your datatypes to be used generically ends here.

-- Some trees to use in the examples
tree1, tree2, tree3 :: Tree Int
tree1 = Leaf 1
tree2 = Branch tree1 (Leaf 2)
tree3 = Branch tree2 (Branch tree2 tree1)

tree4 :: Tree Char
tree4 = Branch (Leaf 'p') (Leaf 'q')

-- A bigger tree created with enum
tree5 :: Tree Bool
tree5 = G.enum !! 1000

-- Showing a tree
example1 = G.show tree3
-- Evaluates to: "Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 1))"

-- Reading a tree
example2 :: Maybe (Tree Int)
example2 = G.read example1
-- G.show example2 evaluates to: "Just (Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 1)))"

-- Zipping two trees
example3 = G.zip tree2 tree4
-- G.show example3 evaluates to: "Just (Branch (Leaf (1,'p')) (Leaf (2,'q')))"

-- Flattening a tree
example4 = G.flattenr tree5
-- Evaluates to: [False,False,False,False,False,False,False,False,False,False]

-- Mapping a function over a tree and zipping it with the original tree
example5 = G.zip tree5 (G.map not tree5)
-- G.show example5 evaluates to: "Just (Branch (Branch (Leaf (False,True)) (Branch (Leaf (False,True)) (Branch (Leaf (False,True)) (Leaf (False,True))))) (Branch (Branch (Branch (Leaf (False,True)) (Leaf (False,True))) (Branch (Leaf (False,True)) (Leaf (False,True)))) (Branch (Leaf (False,True)) (Leaf (False,True)))))"

