{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Functions.Map
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic function that applies a (non-generic) function to all
-- elements contained in a polymorphic datatype.
--
-- 'map' is a generic version of the @Prelude@ @map@ function. It works on all
-- supported container datatypes of kind @* -> *@. The 'map' function is
-- equivalent to 'fmap' after @deriving 'Functor'@ if that were possible.
-----------------------------------------------------------------------------

module Generics.EMGM.Functions.Map (
  Map(..),
  map,
  replace,
  bimap,
) where

import Prelude hiding (map)

import Generics.EMGM.Common

-----------------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------------

-- | The type of a generic function that takes a value of one type and returns a
-- value of a different type.
newtype Map a b = Map { selMap :: a -> b }

-----------------------------------------------------------------------------
-- Generic2 instance declaration
-----------------------------------------------------------------------------

rconstantMap :: a -> a
rconstantMap = id

rsumMap :: Map a1 a2 -> Map b1 b2 -> a1 :+: b1 -> a2 :+: b2
rsumMap ra _  (L a) = L (selMap ra a)
rsumMap _  rb (R b) = R (selMap rb b)

rprodMap :: Map a1 a2 -> Map b1 b2 -> a1 :*: b1 -> a2 :*: b2
rprodMap ra rb (a :*: b) = selMap ra a :*: selMap rb b

rtypeMap :: EP b r -> EP d a -> Map r a -> b -> d
rtypeMap ep1 ep2 ra = to ep2 . selMap ra . from ep1

instance Generic2 Map where
  rconstant2           = Map rconstantMap
  rsum2          ra rb = Map (rsumMap ra rb)
  rprod2         ra rb = Map (rprodMap ra rb)
  rtype2 ep1 ep2 ra    = Map (rtypeMap ep1 ep2 ra)

-----------------------------------------------------------------------------
-- Exported functions
-----------------------------------------------------------------------------

-- | Apply a function to all elements of a container datatype (kind @* -> *@).
map :: (FRep2 Map f) => (a -> b) -> f a -> f b
map = selMap . frep2 . Map

-- | Replace all @a@-values in @as@ with @b@.
replace :: (FRep2 Map f) => f a -> b -> f b
replace as b = map (const b) as

-- | Given a datatype @F a b@, @bimap f g@ applies the function @f :: a -> c@ to
-- every @a@-element and the function @g :: b -> d@ to every @b@-element. The
-- result is a value with transformed elements: @F c d@.
bimap :: (BiFRep2 Map f) => (a -> c) -> (b -> d) -> f a b -> f c d
bimap f g = selMap $ bifrep2 (Map f) (Map g)

