{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE Rank2Types                 #-}
{-# OPTIONS_GHC -fno-warn-unused-binds  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Bimap
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-----------------------------------------------------------------------------

module Bimap (tests) where

import Test.HUnit ((~:))
import Data.Char (ord, chr)

import Base ((~|:))
import Generics.EMGM hiding (Show)
import Generics.EMGM.Derive

--------------------------------------------------------------------------------
-- Fixed-point stuff
--------------------------------------------------------------------------------

-- Note that these are unused for now. They can potentially be useful for
-- enhancing the library, however.

data Fix f a = In (f a (Fix f a))

out :: Fix f a -> f a (Fix f a)
out (In x) = x

fold :: (BiFRep2 Map f) => (f a c -> c) -> Fix f a -> c
fold f = f . bimap id (fold f) . out

unfold :: (BiFRep2 Map f) => (b -> f a b) -> b -> Fix f a
unfold f = In . bimap id (unfold f) . f

hylo :: (BiFRep2 Map f) => (b -> f a b) -> (f a c -> c) -> b -> c
hylo f g = g . bimap id (hylo f g) . f

build :: (BiFRep2 Map f) => (forall b. (f a b -> b) -> b) -> Fix f a
build f = f In

--------------------------------------------------------------------------------
-- Tuple, Either
--------------------------------------------------------------------------------

test_tuple1 = "bimap ord chr ('G', 97)" ~|: bimap ord chr ('G', 97) == (ord 'G', chr 97)

test_either1 = "bimap ord chr (Left 'G')" ~|: bimap ord chr (Left 'G') == Left (ord 'G')
test_either2 = "bimap ord chr (Right 97)" ~|: bimap ord chr (Right 97) == Right (chr 97)

--------------------------------------------------------------------------------
-- ListF
--------------------------------------------------------------------------------

data ListF a b = NilF | ConsF a b
  deriving (Eq, Show)

$(derive ''ListF)

test_list_simple1 = "bimap chr ord (ConsF 4 'a')" ~|: bimap chr ord (ConsF 4 'a') == ConsF (chr 4) (ord 'a')
test_list_simple2 = "bimap chr ord NilF" ~|: bimap chr ord NilF == NilF

--------------------------------------------------------------------------------
-- TreeF
--------------------------------------------------------------------------------

data TreeF a b = TipF a | BinF b b
  deriving (Eq, Show)

$(derive ''TreeF)

test_tree_simple1 = "bimap chr ord (BinF 'c' 'd')" ~|: bimap chr ord (BinF 'c' 'd') == BinF (ord 'c') (ord 'd')
test_tree_simple2 = "bimap chr ord (TipF 1)" ~|: bimap chr ord (TipF 1) == TipF (chr 1)

-----------------------------------------------------------------------------
-- Test collection
-----------------------------------------------------------------------------

tests =
  "Bimap" ~:
    [ test_tuple1
    , test_either1
    , test_either2
    , test_list_simple1
    , test_list_simple2
    , test_tree_simple1
    , test_tree_simple2
    ]

