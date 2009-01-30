{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE OverlappingInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Common.Base
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Type classes used for generic functions with /one/ generic argument.
--
-- Generic functions using one generic argument are defined as instances of
-- 'Generic'. This class contains all of the methods (called \"type cases\" in
-- datatype-generic language) used to define the run-time type representation of
-- a datatype.
--
-- To simplify generic functions, we use type classes for representation
-- dispatching. There are \"dispatchers\" for each category of function (see
-- below) and each category has one \"Rep\" class.
--
-- Some 'Generic'-based functions operate on monomorphic values (using 'Rep').
-- The functions included with the library are:
--
-- * "Generics.EMGM.Functions.Collect"
--
-- * "Generics.EMGM.Functions.Compare"
--
-- * "Generics.EMGM.Functions.Enum"
--
-- * "Generics.EMGM.Functions.Read"
--
-- * "Generics.EMGM.Functions.Show"
--
-- Other 'Generic'-based functions operate on types of the form @f a@ (using
-- 'FRep') where @f@ is the actual generic argument (the one that needs a
-- run-time representation). The functions included with the library are:
--
-- * "Generics.EMGM.Functions.Crush"
-----------------------------------------------------------------------------

module Generics.EMGM.Common.Base (

  -- * Generic function class
  Generic(..),

  -- * Representation dispatcher classes
  Rep(..),
  FRep(..),
) where

import Generics.EMGM.Common.Representation

infixr 5 `rsum`
infixr 6 `rprod`

-- | This class forms the foundation for defining generic functions with a
-- single generic argument. Each method represents a type case. The class
-- includes cases for primitive types, cases for the structural representation,
-- and the 'rtype' case for adding support for new datatypes.
class Generic g where

  -- | Many functions perform the same operation on the non-structural cases (as
  -- well as 'Unit'). The cases for constant datatypes ('Int', 'Integer',
  -- 'Float', 'Double', 'Char', and 'Unit') have a default implementation of
  -- 'rconstant', thus a generic function may only override 'rconstant' if
  -- desired. Note that there is no default implementation for 'rconstant'
  -- itself.
  --
  -- The class context represents the intersection set of supported type
  -- classes.
  rconstant :: (Enum a, Eq a, Ord a, Read a, Show a) => g a

  -- | Case for the primitive type 'Int'. (Default implementation:
  -- 'rconstant'.)
  rint      :: g Int

  -- | Case for the primitive type 'Integer'. (Default implementation:
  -- 'rconstant'.)
  rinteger  :: g Integer

  -- | Case for the primitive type 'Float'. (Default implementation:
  -- 'rconstant'.)
  rfloat    :: g Float

  -- | Case for the primitive type 'Double'. (Default implementation:
  -- 'rconstant'.)
  rdouble   :: g Double

  -- | Case for the primitive type 'Char'. (Default implementation:
  -- 'rconstant'.)
  rchar     :: g Char

  -- | Case for the structural representation type 'Unit'. It is used to
  -- represent a constructor with no arguments. (Default implementation:
  -- 'rconstant'.)
  runit     :: g Unit

  -- | Case for the structural representation type @:+:@ (sum). It
  -- is used to represent alternative choices between constructors. (No
  -- default implementation.)
  rsum      :: g a -> g b -> g (a :+: b)

  -- | Case for the structural representation type @:*:@ (product).
  -- It is used to represent multiple arguments to a constructor. (No
  -- default implementation.)
  rprod     :: g a -> g b -> g (a :*: b)

  -- | Case for constructors. While not necessary for every generic function,
  -- this method is required for 'Read' and 'Show'. It is used to hold the
  -- meta-information about a constructor ('ConDescr'), e.g. name, arity,
  -- fixity, etc. (Since most generic functions do not use 'rcon' and simply pass
  -- the value through, the default implementation is @const id@.)
  rcon      :: ConDescr -> g a -> g a

  -- | Case for datatypes. This method is used to define the structural
  -- representation of an arbitrary Haskell datatype. The first argument is the
  -- embedding-projection pair, necessary for establishing the isomorphism
  -- between datatype and representation. The second argument is the
  -- run-time representation using the methods of 'Generic'. (No default
  -- implementation.)
  rtype     :: EP b a -> g a -> g b

  rint     = rconstant
  rinteger = rconstant
  rfloat   = rconstant
  rdouble  = rconstant
  rchar    = rconstant
  runit    = rconstant

  rcon     = const id

-- | The 'Generic' representation dispatcher for monomorphic types (kind @*@).
-- Every structure type and supported datatype should have an instance of
-- 'Rep'. (No default implementation.)
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

-- | The 'Generic' representation dispatcher for functor types (kind @* -> *@),
-- sometimes called container types. (No default implementation.)
class FRep g f where
  frep :: g a -> g (f a)

