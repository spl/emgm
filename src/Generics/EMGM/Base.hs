-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Base
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Type classes used for generic functions with /one/ generic argument.
--
-- A /generic function/ is defined as an instance of 'Generic', 'Generic2', or
-- 'Generic3'. Each method in the class serves for a case in the datatype
-- representation
--
-- A /representation dispatcher/ simplifies the use of a generic function. There
-- must be an instance of each of the classes 'Rep', 'FRep', 'FRep2', etc. (that
-- apply) for every datatype.
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE OverlappingInstances  #-}

module Generics.EMGM.Base (

  module Generics.EMGM.Representation,

  -- * Classes for Generic Functions

  Generic(..),
  Generic2(..),
  Generic3(..),

  -- * Classes for Representation Dispatchers

  Rep(..),
  FRep(..),
  FRep2(..),
  BiFRep2(..),
  FRep3(..),

) where

import Generics.EMGM.Representation

-- | This class forms the foundation for defining generic functions with a
-- single generic argument. Each method represents a type case. There are cases
-- for primitive types, structural representation types, and for user-defined
-- datatypes.
--
-- The included modules using @Generic@ are:
--
-- * "Generics.EMGM.Functions.Collect"
--
-- * "Generics.EMGM.Functions.Compare"
--
-- * "Generics.EMGM.Functions.Constructor"
--
-- * "Generics.EMGM.Functions.Crush"
--
-- * "Generics.EMGM.Functions.Enum"
--
-- * "Generics.EMGM.Functions.Read"
--
-- * "Generics.EMGM.Functions.Show"

class Generic g where

  -- | Case for the primitive type 'Int'.
  rint      :: g Int

  -- | Case for the primitive type 'Integer'.

  rinteger  :: g Integer

  -- | Case for the primitive type 'Float'.

  rfloat    :: g Float

  -- | Case for the primitive type 'Double'.

  rdouble   :: g Double

  -- | Case for the primitive type 'Char'.

  rchar     :: g Char

  -- | Case for the structural representation type 'Unit'. Represents a
  -- constructor with no arguments.

  runit     :: g Unit

  -- | Case for the structural representation type @:+:@ (sum). Represents
  -- alternative constructors.

  rsum      :: g a -> g b -> g (a :+: b)

  -- | Case for the structural representation type @:*:@ (product). Represents
  -- the fields of a constructor.

  rprod     :: g a -> g b -> g (a :*: b)

  -- | Case for constructors. It is used to hold the meta-information about a
  -- constructor, e.g. name, arity, fixity, etc. This is not needed for many
  -- generic functions, so the default implementation is:
  --
  -- @
  --   rcon = const id
  -- @

  rcon      :: ConDescr -> g a -> g a
  rcon      = const id

  -- | Case for labeled field. Contains the label string. This is not needed for
  -- many generic functions, so the default implementation is:
  --
  -- @
  --   rlbl = const id
  -- @

  rlbl      :: LblDescr -> g a -> g a
  rlbl      = const id

  -- | Case for datatypes. This method is used to define the structural
  -- representation of an arbitrary Haskell datatype. The first argument is the
  -- embedding-projection pair, necessary for establishing the isomorphism
  -- between datatype and representation. The second argument is the
  -- run-time representation using the methods of 'Generic'.

  rtype     :: EP b a -> g a -> g b


infixr 5 `rsum`
infixr 6 `rprod`

-- | This class forms the foundation for defining generic functions with two
-- generic arguments. See 'Generic' for details.
--
-- The included modules using @Generic2@ are:
--
-- * "Generics.EMGM.Functions.Map"
--
-- * "Generics.EMGM.Functions.Transpose"

class Generic2 g where

  rint2      :: g Int Int
  rinteger2  :: g Integer Integer
  rfloat2    :: g Float Float
  rdouble2   :: g Double Double
  rchar2     :: g Char Char
  runit2     :: g Unit Unit
  rsum2      :: g a1 a2 -> g b1 b2 -> g (a1 :+: b1) (a2 :+: b2)
  rprod2     :: g a1 a2 -> g b1 b2 -> g (a1 :*: b1) (a2 :*: b2)

  rcon2      :: ConDescr -> g a1 a2 -> g a1 a2
  rcon2      = const id

  rlbl2      :: LblDescr -> g a1 a2 -> g a1 a2
  rlbl2      = const id

  -- | See 'rtype'. This case is the primary difference that separates
  -- 'Generic2' from 'Generic'. Since we have two generic type parameters, we
  -- need to have two 'EP' values. Each translates between the Haskell type and
  -- its generic representation.
  rtype2     :: EP a2 a1 -> EP b2 b1 -> g a1 b1 -> g a2 b2


infixr 5 `rsum2`
infixr 6 `rprod2`

-- | This class forms the foundation for defining generic functions with three
-- generic arguments. See 'Generic' for details.
--
-- The included modules using @Generic3@ are:
--
-- * "Generics.EMGM.Functions.UnzipWith"
--
-- * "Generics.EMGM.Functions.ZipWith"

class Generic3 g where

  rint3      :: g Int Int Int
  rinteger3  :: g Integer Integer Integer
  rfloat3    :: g Float Float Float
  rdouble3   :: g Double Double Double
  rchar3     :: g Char Char Char
  runit3     :: g Unit Unit Unit
  rsum3      :: g a1 a2 a3 -> g b1 b2 b3 -> g (a1 :+: b1) (a2 :+: b2) (a3 :+: b3)
  rprod3     :: g a1 a2 a3 -> g b1 b2 b3 -> g (a1 :*: b1) (a2 :*: b2) (a3 :*: b3)

  rcon3      :: ConDescr -> g a1 a2 a3 -> g a1 a2 a3
  rcon3      = const id

  rlbl3      :: LblDescr -> g a1 a2 a3 -> g a1 a2 a3
  rlbl3      = const id

  -- | See 'rtype'. This case is the primary difference that separates
  -- 'Generic3' from 'Generic'. Since we have three generic type parameters, we
  -- need three 'EP' values. Each translates between the Haskell type and its
  -- generic representation.
  rtype3     :: EP a2 a1 -> EP b2 b1 -> EP c2 c1 -> g a1 b1 c1 -> g a2 b2 c2


infixr 5 `rsum3`
infixr 6 `rprod3`

-- | Representation dispatcher for monomorphic types (kind @*@) used with
-- 'Generic'. Every structure type and supported datatype should have an
-- instance of 'Rep'.

class Rep g a where
  rep :: g a

instance (Generic g) => Rep g Int where
  rep = rint

instance (Generic g) => Rep g Integer where
  rep = rinteger

instance (Generic g) => Rep g Float where
  rep = rfloat

instance (Generic g) => Rep g Double where
  rep = rdouble

instance (Generic g) => Rep g Char where
  rep = rchar

instance (Generic g) => Rep g Unit where
  rep = runit

instance (Generic g, Rep g a, Rep g b) => Rep g (a :+: b) where
  rep = rsum rep rep

instance (Generic g, Rep g a, Rep g b) => Rep g (a :*: b) where
  rep = rprod rep rep

-- | Representation dispatcher for functor types (kind @* -> *@) used with
-- 'Generic'.

class FRep g f where
  frep :: g a -> g (f a)

-- | Representation dispatcher for functor types (kind @* -> *@) used with
-- 'Generic2'.

class FRep2 g f where
  frep2 :: g a b -> g (f a) (f b)

-- | Representation dispatcher for bifunctor types (kind @* -> *@) used with
-- 'Generic2'.

class BiFRep2 g f where
  bifrep2 :: g a1 b1 -> g a2 b2 -> g (f a1 a2) (f b1 b2)

-- | Representation dispatcher for functor types (kind @* -> *@) used with
-- 'Generic3'.

class FRep3 g f where
  frep3 :: g a b c -> g (f a) (f b) (f c)

