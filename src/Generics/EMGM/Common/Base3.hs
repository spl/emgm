{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Common.Base3
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Type classes used for generic functions with /three/ generic
-- arguments.
--
-- Generic functions using three generic arguments are defined as instances of
-- 'Generic3'. This class contains all of the methods (called \"type cases\" in
-- datatype-generic language) used to define the run-time type representation of
-- a datatype.
--
-- 'Generic3'-based functions have a non-extensible representation dispatcher
-- type class, 'FRep3'.
--
-- The functions included with the library are:
--
-- * "Generics.EMGM.Functions.UnzipWith"
--
-- * "Generics.EMGM.Functions.ZipWith"
-----------------------------------------------------------------------------

module Generics.EMGM.Common.Base3 (

  -- * Generic function class
  Generic3(..),

  -- * Representation dispatcher classes
  FRep3(..),
) where

import Generics.EMGM.Common.Representation

infixr 5 `rsum3`
infixr 6 `rprod3`

-- | This class forms the foundation for defining generic functions with three
-- generic arguments. Each method represents a type case. The class includes
-- cases for primitive types, cases for the structural representation, and the
-- 'rtype' case for adding support for new datatypes.
class Generic3 g where

  -- | Many functions perform the same operation on the non-structural cases (as
  -- well as 'Unit'). The cases for constant datatypes ('Int', 'Integer',
  -- 'Float', 'Double', 'Char', and 'Unit') have a default implementation of
  -- 'rconstant3', thus a generic function may only override 'rconstant3' if
  -- desired. Note that there is no default implementation for 'rconstant3'
  -- itself.
  --
  -- The class context represents the intersection set of supported type
  -- classes.
  rconstant3 :: (Enum a, Eq a, Ord a, Read a, Show a) => g a a a

  -- | Case for the primitive type 'Int'. (Default implementation:
  -- 'rconstant3'.)
  rint3      :: g Int Int Int

  -- | Case for the primitive type 'Integer'. (Default implementation:
  -- 'rconstant3'.)
  rinteger3  :: g Integer Integer Integer

  -- | Case for the primitive type 'Float'. (Default implementation:
  -- 'rconstant3'.)
  rfloat3    ::  g Float Float Float

  -- | Case for the primitive type 'Double'. (Default implementation:
  -- 'rconstant3'.)
  rdouble3   ::  g Double Double Double

  -- | Case for the primitive type 'Char'. (Default implementation:
  -- 'rconstant3'.)
  rchar3     ::  g Char Char Char

  -- | Case for the structural representation type 'Unit'. It is used to
  -- represent a constructor with no arguments. (Default implementation:
  -- 'rconstant3'.)
  runit3     :: g Unit Unit Unit

  -- | Case for the structural representation type @:+:@ (sum). It
  -- is used to represent alternative choices between constructors. (No
  -- default implementation.)
  rsum3      :: g a1 a2 a3 -> g b1 b2 b3 -> g (a1 :+: b1) (a2 :+: b2) (a3 :+: b3)

  -- | Case for the structural representation type @:*:@ (product).
  -- It is used to represent multiple arguments to a constructor. (No
  -- default implementation.)
  rprod3     :: g a1 a2 a3 -> g b1 b2 b3 -> g (a1 :*: b1) (a2 :*: b2) (a3 :*: b3)

  -- | Case for constructors. It is used to hold the meta-information about a
  -- constructor ('ConDescr'), e.g. name, arity, fixity, etc. (Since most
  -- generic functions do not use 'rcon' and simply pass the value through, the
  -- default implementation is @const id@.)
  rcon3      :: ConDescr -> g a1 a2 a3 -> g a1 a2 a3

  -- | Case for datatypes. This method is used to define the structural
  -- representation of an arbitrary Haskell datatype. The first three arguments
  -- are the embedding-projection pairs, necessary for establishing the
  -- isomorphisms between datatype and representation of the four generic types.
  -- The fourth argument is the run-time representation using the methods of
  -- 'Generic3'. (No default implementation.)
  rtype3     :: EP a2 a1 -> EP b2 b1 -> EP c2 c1 -> g a1 b1 c1 -> g a2 b2 c2

  rint3      = rconstant3
  rinteger3  = rconstant3
  rfloat3    = rconstant3
  rdouble3   = rconstant3
  rchar3     = rconstant3
  runit3     = rconstant3

  rcon3      = const id

-- | The 'Generic3' representation dispatcher for functor types (kind @* -> *@),
-- sometimes called container types. (No default implementation.)
class FRep3 g f where
  frep3 :: g a b c -> g (f a) (f b) (f c)

