-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- EMGM is \"Extensible and Modular Generics for the Masses,\" a library for
-- datatype-generic programming in Haskell.
--
-- This module exports the most commonly used types, classes, and functions. The
-- documentation is organized by topic for convenient access.
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

module Generics.EMGM (

  -- * Foundation
  --
  -- | This is the collection of types, classes, and functions used to define
  -- generic functions and to build representations for datatypes.

  -- ** Datatype Representation
  --
  -- | These are the types and functions required to represent a datatype for
  -- use by generic functions.

  -- *** Structure Representation Types
  --
  -- | The unit, sum, and product types form the sum-of-products view for a
  -- Haskell datatype.

  Unit(..),
  (:+:)(..),
  (:*:)(..),

  -- *** Embedding-Projection Pair
  --
  -- | A pair of a function and its inverse form the isomorphism between a
  -- datatype and its structure representation.

  EP(..),

  -- *** Constructor Description
  --
  -- | A description of the syntax of each constructor provides useful auxiliary
  -- information for some generic functions.

  ConDescr(..),
  LblDescr(..),
  Fixity(..),
  Associativity(..),
  Prec,
  prec,

  -- ** Generic Function Classes
  --
  -- | Generic functions are instances of these classes. The value-level
  -- structure representation of datatypes is implemented using the members of
  -- these classes. Thus, a generic function is simply a case statement on the
  -- value-level structure.
  --
  -- Note that the numerical suffix represents the number of generic type
  -- variables used in the generic function. No suffix represents 1 generic type
  -- variable.

  Generic(..),
  Generic2(..),
  Generic3(..),

  -- ** Representation Dispatchers
  --
  -- | Type classes simplify the application of generic functions by providing
  -- (a.k.a. \"dispatching\") the appropriate structure representation. These
  -- classes are divided into the kinds they support (monomorphic, functor, and
  -- bifunctor).
  --
  -- Note that the numerical suffix represents the number of generic type
  -- variables used in the generic function. No suffix represents 1 generic type
  -- variable.

  -- *** Monomorphic
  --
  -- | All types of kind @*@ should have an instance here. This includes types
  -- applied to type variables: @[a]@, @'Maybe' a@, @'Either' a b@, etc.

  Rep(..),

  -- *** Functor
  --
  -- | Types of kind @* -> *@ should have an instance here. This includes @[]@,
  -- 'Maybe', etc.

  FRep(..),
  FRep2(..),
  FRep3(..),

  -- *** Bifunctor
  --
  -- | Types of kind @* -> * -> *@ should have an instance here. This includes
  -- @(,)@, 'Either', etc.

  BiFRep2(..),

  -- * Generic Functions
  --
  -- | The following collection of functions use the common EMGM infrastructure
  -- to work on all datatypes that have instances for a certain representation
  -- dispatcher. These functions are categorized by the core generic
  -- functionality. For example, 'flattenr' is a type of \"crush\" function,
  -- because it is defined by the 'Generic' instance of the @newtype 'Crush'@.
  --
  -- More information for each of these is available in its respective module.

  -- ** Collect
  --
  -- | Function that collects values of one type from values of a possibly
  -- different type.
  --
  -- For more details, see "Generics.EMGM.Functions.Collect".

  collect,

  -- ** Compare
  --
  -- | Functions that compare two values to determine an ordering.
  --
  -- For more details, see "Generics.EMGM.Functions.Compare".

  compare,
  eq,
  neq,
  lt,
  lteq,
  gt,
  gteq,
  min,
  max,

  -- ** Crush
  --
  -- | Functions that crush a polymorphic functor container into an iteration
  -- over its elements.
  --
  -- For more details, see "Generics.EMGM.Functions.Crush".

  Assoc(..),
  crush,
  crushl,
  crushr,
  flatten,
  flattenl,
  flattenr,
  first,
  firstl,
  firstr,
  and,
  or,
  any,
  all,
  sum,
  product,
  minimum,
  maximum,
  elem,
  notElem,

  -- ** Enum
  --
  -- | Functions that enumerate the values of a datatype.
  --
  -- For more details, see "Generics.EMGM.Functions.Enum".

  enum,
  enumN,
  empty,

  -- ** Everywhere
  --
  -- | Functions that apply a transformation at every location of one type in a
  -- value of a possibly different type.
  --
  -- For more details, see "Generics.EMGM.Functions.Everywhere".

  everywhere,
  everywhere',

  -- ** Map
  --
  -- | Functions that translate values of one type to values of another. This
  -- includes map-like functions that apply non-generic functions to every
  -- element in a polymorphic (functor or bifunctor) container. It also includes
  -- 'cast', a configurable, type-safe casting function.
  --
  -- For more details, see "Generics.EMGM.Functions.Map".

  map,
  replace,
  bimap,
  cast,

  -- ** Meta
  --
  -- | Functions for extracting meta-information about the representation.
  --
  -- For more details, see "Generics.EMGM.Functions.Meta".

  HasEP(..),
  conDescr,
  lblDescrs,

  -- ** Read
  --
  -- | Functions similar to @deriving Prelude.Read@ that parse a string and return a
  -- value of a datatype.
  --
  -- For more details, see "Generics.EMGM.Functions.Read".

  readPrec,
  readP,
  readsPrec,
  reads,
  read,

  -- ** Show
  --
  -- | Functions similar to @deriving Prelude.Show@ that return a string
  -- representation of a value of a datatype.
  --
  -- For more details, see "Generics.EMGM.Functions.Show".

  showsPrec,
  shows,
  show,

  -- ** Transpose
  --
  -- | Functions that transpose polymorphic functor values.
  --
  -- For more details, see "Generics.EMGM.Functions.Transpose".

  transpose,
  transposeE,

  -- ** UnzipWith
  --
  -- | Functions that split a polymorphic functor values into two structurally
  -- equilvalent values.
  --
  -- For more details, see "Generics.EMGM.Functions.UnzipWith".

  unzipWithM,
  unzipWith,
  unzip,

  -- ** ZipWith
  --
  -- | Functions that combine two structurally equilvalent, polymorphic functor
  -- values into one.
  --
  -- For more details, see "Generics.EMGM.Functions.ZipWith".

  zipWithM,
  zipWith,
  zip,

) where

import qualified Prelude ()

import Generics.EMGM.Base
import Generics.EMGM.Functions.Collect
import Generics.EMGM.Functions.Compare
import Generics.EMGM.Functions.Crush
import Generics.EMGM.Functions.Enum
import Generics.EMGM.Functions.Everywhere
import Generics.EMGM.Functions.Map
import Generics.EMGM.Functions.Meta
import Generics.EMGM.Functions.Read
import Generics.EMGM.Functions.Show
import Generics.EMGM.Functions.Transpose
import Generics.EMGM.Functions.UnzipWith
import Generics.EMGM.Functions.ZipWith

-- Export the instances from these
import Generics.EMGM.Data.Bool()
import Generics.EMGM.Data.Either()
import Generics.EMGM.Data.List()
import Generics.EMGM.Data.Maybe()
import Generics.EMGM.Data.Tuple()

