{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Common.Base2
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Type classes used for generic functions with /two/ generic
-- arguments.
--
-- Generic functions using two generic arguments are defined as instances of
-- 'Generic2'. This class contains all of the methods (called \"type cases\" in
-- datatype-generic language) used to define the run-time type representation of
-- a datatype.
--
-- 'Generic2'-based functions have a representation dispatcher type class
-- 'FRep2'.
--
-- The functions included with the library are:
--
-- * "Generics.EMGM.Functions.Map"
-----------------------------------------------------------------------------

module Generics.EMGM.Common.Base2 (

  -- * Generic function class
  Generic2(..),

  -- * Representation dispatcher classes
  FRep2(..),
  BiFRep2(..),
) where

import Generics.EMGM.Common.Representation

infixr 5 `rsum2`
infixr 6 `rprod2`

-- | This class forms the foundation for defining generic functions with two
-- generic arguments. Each method represents a type case. The class includes
-- cases for primitive types, cases for the structural representation, and the
-- 'rtype' case for adding support for new datatypes.
class Generic2 g where

  -- | Many functions perform the same operation on the non-structural cases (as
  -- well as 'Unit'). The cases for constant datatypes ('Int', 'Integer',
  -- 'Float', 'Double', 'Char', and 'Unit') have a default implementation of
  -- 'rconstant2', thus a generic function may only override 'rconstant2' if
  -- desired. Note that there is no default implementation for 'rconstant2'
  -- itself.
  --
  -- The class context represents the intersection set of supported type
  -- classes.
  rconstant2 :: (Enum a, Eq a, Ord a, Read a, Show a) => g a a

  -- | Case for the primitive type 'Int'. (Default implementation:
  -- 'rconstant2'.)
  rint2      :: g Int Int

  -- | Case for the primitive type 'Integer'. (Default implementation:
  -- 'rconstant2'.)
  rinteger2  :: g Integer Integer

  -- | Case for the primitive type 'Float'. (Default implementation:
  -- 'rconstant2'.)
  rfloat2    ::  g Float Float

  -- | Case for the primitive type 'Double'. (Default implementation:
  -- 'rconstant2'.)
  rdouble2   ::  g Double Double

  -- | Case for the primitive type 'Char'. (Default implementation:
  -- 'rconstant2'.)
  rchar2     ::  g Char Char

  -- | Case for the structural representation type 'Unit'. It is used to
  -- represent a constructor with no arguments. (Default implementation:
  -- 'rconstant2'.)
  runit2     :: g Unit Unit

  -- | Case for the structural representation type @:+:@ (sum). It
  -- is used to represent alternative choices between constructors. (No
  -- default implementation.)
  rsum2      :: g a1 a2 -> g b1 b2 -> g (a1 :+: b1) (a2 :+: b2)

  -- | Case for the structural representation type @:*:@ (product).
  -- It is used to represent multiple arguments to a constructor. (No
  -- default implementation.)
  rprod2     :: g a1 a2 -> g b1 b2 -> g (a1 :*: b1) (a2 :*: b2)

  -- | Case for constructors. It is used to hold the meta-information about a
  -- constructor ('ConDescr'), e.g. name, arity, fixity, etc. (Since most
  -- generic functions do not use 'rcon' and simply pass the value through, the
  -- default implementation is @const id@.)
  rcon2      :: ConDescr -> g a1 a2 -> g a1 a2

  -- | Case for datatypes. This method is used to define the structural
  -- representation of an arbitrary Haskell datatype. The first two arguments
  -- are the embedding-projection pairs, necessary for establishing the
  -- isomorphisms between datatype and representation of the two generic types.
  -- The third argument is the run-time representation using the methods of
  -- 'Generic2'. (No default implementation.)
  rtype2     :: EP a2 a1 -> EP b2 b1 -> g a1 b1 -> g a2 b2

  rint2      = rconstant2
  rinteger2  = rconstant2
  rfloat2    = rconstant2
  rdouble2   = rconstant2
  rchar2     = rconstant2
  runit2     = rconstant2

  rcon2      = const id

-- | The 'Generic2' representation dispatcher for functor types (kind @* -> *@),
-- sometimes called container types. (No default implementation.)
class FRep2 g f where
  frep2 :: g a b -> g (f a) (f b)

-- | The 'Generic2' representation dispatcher for bifunctor types (kind
-- @* -> * -> *@). (No default implementation.)
class BiFRep2 g f where
  bifrep2 :: g a1 b1 -> g a2 b2 -> g (f a1 a2) (f b1 b2)

