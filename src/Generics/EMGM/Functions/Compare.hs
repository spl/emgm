{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Functions.Compare
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic functions for comparing two values in different ways.
--
-- The fundamental function here is 'compare', a function that returns the
-- 'Ordering' of two values (less than, equal to, or greater than). It uses the
-- same lexicographical ordering as @deriving Ord@ (e.g. left alternative of a
-- sum is less than the right alternative, the first component of a product is
-- compared first while the second is only compared if the first is equal,
-- etc.).
--
-- All of the remaining functions are simply derived (in the most obvious way)
-- from 'compare'. All of these functions are equivalent to methods in the 'Eq'
-- and 'Ord' type classes. The difference with using this approach vs. @deriving
-- (Eq, Ord)@ is that you can write ad-hoc cases for certain datatypes while
-- most of the functionality is handled generically.
-----------------------------------------------------------------------------

module Generics.EMGM.Functions.Compare (

  -- * Compare
  -- | 'compare' is equivalent to the function of the same name when @deriving
  -- Ord@. All other comparison functions in this module are derived from
  -- 'compare'.
  Compare(..),
  compare,

  -- * Equality, inequality
  -- | These functions are equivalent to @(==)@ and @(/=)@ when @deriving Eq@.
  eq,
  neq,

  -- * Less than, greater than
  -- | These functions are equivalent to @(\<)@, @(\<=)@, @(>)@, and @(>=)@ when
  -- @deriving Ord@.
  lt,
  lteq,
  gt,
  gteq,

  -- * Minimum and maximum
  -- | These functions are equivalent to functions of the same name when
  -- @deriving Ord@.
  min,
  max,
) where

import Prelude hiding (compare, min, max)
import qualified Prelude as P (compare)

import Generics.EMGM.Common.Base

-----------------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------------

-- | The type of a generic function that takes two values of the same type and
-- returns an 'Ordering'.
newtype Compare a = Compare { selCompare :: a -> a -> Ordering }

-----------------------------------------------------------------------------
-- Generic instance declaration
-----------------------------------------------------------------------------

rconstantCompare :: (Ord a) => a -> a -> Ordering
rconstantCompare = P.compare

rsumCompare :: Compare a -> Compare b -> a :+: b -> a :+: b -> Ordering
rsumCompare ra _  (L a1) (L a2) = {-EQ-} selCompare ra a1 a2
rsumCompare _  rb (R b1) (R b2) = {-EQ-} selCompare rb b1 b2
rsumCompare _  _  (L _)  (R _)  =   LT
rsumCompare _  _  (R _)  (L _)  =   GT

rprodCompare :: Compare a -> Compare b -> a :*: b -> a :*: b -> Ordering
rprodCompare ra rb (a1 :*: b1) (a2 :*: b2) =
  case selCompare ra a1 a2 of
    EQ    -> selCompare rb b1 b2
    other -> other

rconCompare :: ConDescr -> Compare a -> a -> a -> Ordering
rconCompare _ = selCompare

rtypeCompare :: EP a b -> Compare b -> a -> a -> Ordering
rtypeCompare ep rb a1 a2 = selCompare rb (from ep a1) (from ep a2)

instance Generic Compare where
  rconstant      = Compare rconstantCompare
  rsum     ra rb = Compare (rsumCompare ra rb)
  rprod    ra rb = Compare (rprodCompare ra rb)
  rcon  cd ra    = Compare (rconCompare cd ra)
  rtype ep ra    = Compare (rtypeCompare ep ra)

-----------------------------------------------------------------------------
-- Exported functions
-----------------------------------------------------------------------------

-- Set the fixity and precedence the same as the infix operators according to
-- the Haskell Report: http://www.haskell.org/onlinereport/decls.html
infix 4 `compare`, `lt`, `lteq`, `eq`, `neq`, `gt`, `gteq`, `min`, `max`

-- | Compare two values and return an 'Ordering' (i.e. @LT@, @GT@, or @EQ@).
-- This is implemented exactly as if the datatype was @deriving Ord@.
compare :: (Rep Compare a) => a -> a -> Ordering
compare = selCompare rep

-- | Less than. Returns @x < y@.
lt :: (Rep Compare a) => a -> a -> Bool
lt x y =
  case compare x y of
    LT -> True
    _  -> False

-- | Less than or equal to. Returns @x <= y@.
lteq :: (Rep Compare a) => a -> a -> Bool
lteq x y =
  case compare x y of
    GT -> False
    _  -> True

-- | Equal to. Returns @x == y@.
eq :: (Rep Compare a) => a -> a -> Bool
eq x y =
  case compare x y of
    EQ -> True
    _  -> False

-- | Not equal to. Returns @x /= y@.
neq :: (Rep Compare a) => a -> a -> Bool
neq x y =
  case compare x y of
    EQ -> False
    _  -> True

-- | Greater than. Returns @x > y@.
gt :: (Rep Compare a) => a -> a -> Bool
gt x y =
  case compare x y of
    GT -> True
    _  -> False

-- | Greater than or equal to. Returns @x >= y@.
gteq :: (Rep Compare a) => a -> a -> Bool
gteq x y =
  case compare x y of
    LT -> False
    _  -> True

-- | The minimum of two values.
min :: (Rep Compare a) => a -> a -> a
min x y = if x `lteq` y then x else y

-- | The maximum of two values.
max :: (Rep Compare a) => a -> a -> a
max x y = if x `gteq` y then x else y

