{-# LANGUAGE CPP                    #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM
-- Copyright   :  (c) 2008 Universiteit Utrecht
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
--
-- For more in-depth documentation, refer to one of the modules in these
-- hierarchies:
--
-- * "Generics.EMGM.Common" - Common infrastructure for supporting datatypes and
-- defining functions.
--
-- * "Generics.EMGM.Functions" - Generic functions included with EMGM.
--
-- * "Generics.EMGM.Derive" - Generating the EMGM representation for a datatype.
-----------------------------------------------------------------------------

module Generics.EMGM (

  -- * Common Infrastructure
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
  ConType(..),

  Fixity(..),
  prec,
  minPrec,
  maxPrec,
  appPrec,
  recPrec,

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

  -- ** Generic Function Definition
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

  -- ** Deriving Representation
  --
  -- | The necessary values and instances for using EMGM with a user-defined
  -- datatype can be generated automatically using Template Haskell. By
  -- necessity, there are a number of exported values for this process that are
  -- unrelated to other uses of the EMGM library. In order to not export these
  -- signatures more than necessary, you should import "Generics.EMGM.Derive"
  -- for deriving the representation. Note that "Generics.EMGM" does not export
  -- anything in "Generics.EMGM.Derive".

  -- * Generic Functions
  --
  -- | The following collection of functions use the common EMGM infrastructure
  -- to work on all datatypes that have instances for a certain representation
  -- dispatcher. These functions are categorized by the core generic
  -- functionality. For example, 'flattenr' is a type of \"crush\" function,
  -- because it is defined by the 'Generic' instance of the @newtype 'Crush'@.
  --
  -- More information for each of these is available in its respective module.

  -- ** Collect Function
  --
  -- | Function that collects values of one type from values of a possibly
  -- different type.
  --
  -- For more details, see "Generics.EMGM.Functions.Collect".

  Collect(..),

  collect,

  -- ** Compare Functions
  --
  -- | Functions that compare two values to determine an ordering.
  --
  -- For more details, see "Generics.EMGM.Functions.Compare".

  Compare(..),

  compare,

  eq,
  neq,

  lt,
  lteq,

  gt,
  gteq,

  min,
  max,

  -- ** Crush Functions
  --
  -- | Functions that crush a polymorphic functor container into an iteration
  -- over its elements.
  --
  -- For more details, see "Generics.EMGM.Functions.Crush".

  Crush(..),
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

  -- ** Enum Functions
  --
  -- | Functions that enumerate the values of a datatype.
  --
  -- For more details, see "Generics.EMGM.Functions.Enum".

  Enum(..),

  enum,
  enumN,

  empty,

  -- ** Everywhere Functions
  --
  -- | Functions that apply a transformation at every location of one type in a
  -- value of a possibly different type.
  --
  -- For more details, see "Generics.EMGM.Functions.Everywhere".

  Everywhere(..),

  everywhere,

  Everywhere'(..),

  everywhere',

  -- ** Map Functions
  --
  -- | Functions that translate values of one type to values of another. This
  -- includes map-like functions that apply non-generic functions to every
  -- element in a polymorphic (functor or bifunctor) container. It also includes
  -- 'cast', a configurable, type-safe casting function.
  --
  -- For more details, see "Generics.EMGM.Functions.Map".

  Map(..),

  map,

  replace,

  bimap,

  cast,

  -- ** Read Functions
  --
  -- | Functions similar to @deriving 'Prelude.Read'@ that parse a string and return a
  -- value of a datatype.
  --
  -- For more details, see "Generics.EMGM.Functions.Read".

  Read(..),

  readPrec,
  readP,

  readsPrec,
  reads,

  read,

  -- ** Show Functions
  --
  -- | Functions similar to @deriving 'Prelude.Show'@ that return a string
  -- representation of a value of a datatype.
  --
  -- For more details, see "Generics.EMGM.Functions.Show".

  Show(..),

  showsPrec,
  shows,

  show,

  -- ** UnzipWith Functions
  --
  -- | Functions that split a polymorphic functor values into two structurally
  -- equilvalent values.
  --
  -- For more details, see "Generics.EMGM.Functions.UnzipWith".

  UnzipWith(..),

  unzip,
  unzipWith,

  -- ** ZipWith Functions
  --
  -- | Functions that combine two structurally equilvalent, polymorphic functor
  -- values into one.
  --
  -- For more details, see "Generics.EMGM.Functions.ZipWith".

  ZipWith(..),

  zip,
  zipWith,

) where

import qualified Prelude

import Generics.EMGM.Common
import Generics.EMGM.Functions

-- Export the instances from these
import Generics.EMGM.Data.Bool()
import Generics.EMGM.Data.Either()
import Generics.EMGM.Data.List()
import Generics.EMGM.Data.Maybe()
import Generics.EMGM.Data.Tuple()

