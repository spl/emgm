{-# LANGUAGE TypeOperators #-}

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

module Generics.EMGM.Representation (

  -- * Structure Representation
  --
  -- | The unit, sum, and product types form the sum-of-products view for a
  -- Haskell datatype.

  Unit(..),
  (:+:)(..),
  (:*:)(..),

  -- * Embedding-Projection Pair
  --
  -- | A pair of a function and its inverse form the isomorphism between a
  -- datatype and its structure representation.

  EP(..),

  -- * Constructor Description
  --
  -- | A description of the syntax of each constructor provides useful auxiliary
  -- information for some generic functions.

  ConDescr(..),
  ConType(..),

  -- * Fixity and Precedence
  -- | These are used to determine whether a constructor is infix or not and, if
  -- it is infix, what its associativity and precedence are.

  Fixity(..),
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

-- | The \"unit\" encodes a constructor with no arguments. An analogous standard
-- Haskell type is @()@.
data Unit
  = Unit -- ^ The only value of type @Unit@ (ignoring @_|_@).
    deriving (Enum, Eq, Ord)

-- | The Read instance for Unit should always return a value and consume nothing
-- of the input, because there is no string representation for it. This allows
-- us to use 'readPrec' in the 'rconstant' method of the generic 'Read'
-- definition.
instance Read Unit where
  readsPrec _ s = [(Unit, s)]

-- | The Show instance for Unit should return an empty string, because there is
-- no representation for it. This allows us to use 'showsPrec' in the
-- 'rconstant' method of the generic 'Show' definition.
instance Show Unit where
  showsPrec _ _ = id

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

-- | The embedding-projection pair contains two functions for converting between
-- the datatype and its representation. An @EP@ value preserves an isomorphism
-- (ignoring @_|_@s) between a datatype and its structure representation.
data EP d r
  = EP
    { from :: (d -> r) -- ^ Embed a @d@atatype into its @r@epresentation.
    , to   :: (r -> d) -- ^ Project @d@atatype from its @r@epresentation.
    }

-- | A constructor description containing useful meta-information about the
-- syntax used in the data declaration. This is particularly useful in 'Read'
-- and 'Show' but may also be helpful in other generic functions.
--
-- NOTE: It is important that the 'ConDescr' value accurately describe the
-- syntax in a constructor declaration. An incorrect description may lead to
-- faulty 'Read' or 'Show' operation.
data ConDescr
  = ConDescr
    { conName     :: String   -- ^ Name of the constructor. If it is infix,
                              -- don't provide parentheses.

    , conArity    :: Int      -- ^ Arity or number of arguments.

    , conLabels   :: [String] -- ^ A list of labels used in record syntax.
                              -- They must be declared in the same order as
                              -- the @data@ declaration. The list should be
                              -- empty if the constructor is not a record.

    , conFixity   :: Fixity   -- ^ Infix or not, associativity, precedence.
    }
  deriving (Eq, Show)

-- | The constructor type used in 'Read' and 'Show' to determine how to parse or
-- print the constructor.
data ConType
  = ConStd             -- ^ Standard (function-type, nonfix)
  | ConRec [String]    -- ^ Record-style (nonfix or infix)
  | ConIfx String      -- ^ Infix (no record syntax)
  deriving (Eq, Show)

-- TODO: Need smart constructor(s) for ConDescr, so we can verify things.

-- | An identifier's fixity, associativity, and precedence. If not infix
-- ('Nonfix'), the associativity and precedence of the identifier is the same as
-- function application. If infix, the associativity is indicated by the
-- constructor and the precedence is an argument to it.
data Fixity
  = Nonfix      -- ^ Not infix. Associativity and precedence are the same as function application.
  | Infix Prec  -- ^ Non-associative infix with precedence.
  | Infixl Prec -- ^ Left-associative infix with precedence.
  | Infixr Prec -- ^ Right-associative Infix with precedence.
  deriving (Eq, Show)

-- | Get the precedence of a fixity value.
prec :: Fixity -> Prec
prec Nonfix     = appPrec
prec (Infix  n) = n
prec (Infixl n) = n
prec (Infixr n) = n

-- | Maximum precedence: 11
maxPrec :: Prec
maxPrec = 11

-- | Precedence for function application: 10
appPrec :: Prec
appPrec = 10

-- | Precedence for record construction: 11
recPrec :: Prec
recPrec = appPrec + 1

