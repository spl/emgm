-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Representation
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Types and related functions for the representation used in EMGM.
--
-- EMGM uses a generic sum-of-products view of datatypes encoded into the
-- 'Unit', @:+:@ (sum), and @:*:@ (product). Many Haskell datatypes can be
-- represented in this way. Right-nested sums replace the @|@, and right-nested
-- products replace the arguments to a constructor. Units replace constructors
-- with no arguments.
--
-- Since constructors encode more than just a list of arguments, this library
-- uses 'ConDescr' to store that information. This includes name, arity, record
-- labels, fixity, and operator precedence. Constructor descriptions are useful
-- for generic operations such as 'Read' and 'Show' and possibly others.
--
-- Generic functions need to convert values between the Haskell datatype and its
-- structure representation. This is done using the embedding-projection pair,
-- which is simply a pair a functions for translating between two types.
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}

module Generics.EMGM.Representation (

  -- * Structure Representation
  --
  -- | The unit, sum, and product types form the sum-of-products view for a
  -- Haskell datatype.

  Unit(..),
  (:+:)(..),
  (:*:)(..),

  -- * Constructor Description
  --
  -- | A description of the syntax of each constructor provides useful auxiliary
  -- information for some generic functions.

  ConDescr(..),
  ConType(..),
  LblDescr(..),

  -- * Embedding-Projection Pair
  --
  -- | A pair of a function and its inverse form the isomorphism between a
  -- datatype and its structure representation.

  EP(..),

  -- * Fixity and Precedence
  -- | These are used to determine whether a constructor is infix or not and, if
  -- it is infix, what its associativity and precedence are.

  Fixity(..),
  Associativity(..),
  Prec,
  prec,
  minPrec,
  maxPrec,
  appPrec,
  recPrec,

  ) where

import Text.ParserCombinators.ReadPrec (minPrec, Prec)

infixr 5 :+:
infixr 6 :*:

-- | Encodes a constructor with no arguments. An analogous standard Haskell type
-- is @()@.

data Unit
  = Unit -- ^ The only value of type @Unit@ (ignoring @_|_@).
    deriving (Enum, Eq, Ord, Show)

-- | The \"sum\" encodes 2 constructor alternatives. An analogous standard
-- Haskell type is @'Either' a b@.
data a :+: b
  = L a -- ^ Left alternative
  | R b -- ^ Right alternative
  deriving (Eq, Ord, Read, Show)

-- | The \"product\" encodes 2 constructor arguments. An analogous standard
-- Haskell type is @(a, b)@.
data a :*: b
  = a :*: b -- ^ A pair of arguments
  deriving (Eq, Ord, Read, Show)

-- | Encodes the string label for a field in a constructor defined with labeled
-- fields (a.k.a. record syntax).

newtype LblDescr = LblDescr String
  deriving (Eq, Ord, Read, Show)

-- | The embedding-projection pair contains two functions for converting between
-- the datatype and its representation. An @EP@ value preserves an isomorphism
-- (ignoring @_|_@s) between a datatype and its structure representation.
data EP d r
  = EP
    { from :: (d -> r) -- ^ Embed a @d@atatype into its @r@epresentation.
    , to   :: (r -> d) -- ^ Project @d@atatype from its @r@epresentation.
    }

-- | Contains useful meta-information about the syntax used in a constructor
-- declaration.
--
-- NOTE: It is important that the 'ConDescr' value accurately describe the
-- syntax in a constructor declaration. An incorrect description may lead to
-- faulty 'Read' or 'Show' operation.

data ConDescr
  = ConDescr
    { -- | Name of the constructor (without parenthesese if infix).
      conName     :: String,

      -- | Number of fields.
      conArity    :: Int,

      -- | Uses labeled fields (a.k.a. record syntax).
      conRecord   :: Bool,

      -- | Fixity, associativity, precedence.
      conFixity   :: Fixity
    }
  deriving (Eq, Show)

-- | Type of constructor syntax. Used in the generic functions 'Read' and
-- 'Show'.

data ConType
  = UnknownC       -- ^ Have not seen the rcon yet
  | NormalC        -- ^ Normal prefix-style constructor
  | InfixC String  -- ^ Infix with symbol (no record syntax)
  | RecordC        -- ^ Record-style (any fixity)
  deriving (Eq, Show)

-- | A constructor's fixity, associativity, and precedence.
data Fixity
  -- | Associativity and precedence are the same as function application.
  = Prefix
  | Infix Associativity Prec
  deriving (Eq, Ord, Read, Show)

-- | A constructor's associativity.
data Associativity
  -- | Declared with infixl
  = LeftAssoc

  -- | Declared with infixr
  | RightAssoc

  -- | Declared with infix
  | NonAssoc
  deriving (Eq, Ord, Read, Show)

-- TODO: Need smart constructor(s) for ConDescr, so we can verify things.

-- | Get the precedence of a fixity value.
prec :: Fixity -> Prec
prec Prefix     = appPrec
prec (Infix _ n) = n

-- | Maximum precedence: 11
maxPrec :: Prec
maxPrec = recPrec

-- | Precedence for function application: 10
appPrec :: Prec
appPrec = 10

-- | Precedence for record construction: 11
recPrec :: Prec
recPrec = appPrec + 1

