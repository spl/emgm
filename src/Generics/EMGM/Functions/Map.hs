-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Functions.Map
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic functions that translate values of one type into values of
-- another.
--
-- 'map' is a generic version of the @Prelude@ @map@ function. It works
-- on all supported container datatypes of kind @* -> *@. The 'map' function is
-- equivalent to 'fmap' after @deriving 'Functor'@ if that were possible.
--
-- 'cast' is a generic and configurable function for converting a value of one
-- type into a value of another using instances provided by the programmer.
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}

module Generics.EMGM.Functions.Map (
  Map(..),
  map,
  bimap,
  cast,
) where

import Prelude hiding (map)

import Generics.EMGM.Base

-----------------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------------

-- | The type of a generic function that takes a value of one type and returns a
-- value of a different type.
newtype Map a b = Map { selMap :: a -> b }

-----------------------------------------------------------------------------
-- Generic2 instance declaration
-----------------------------------------------------------------------------

rsumMap :: Map a1 a2 -> Map b1 b2 -> a1 :+: b1 -> a2 :+: b2
rsumMap ra _  (L a) = L (selMap ra a)
rsumMap _  rb (R b) = R (selMap rb b)

rprodMap :: Map a1 a2 -> Map b1 b2 -> a1 :*: b1 -> a2 :*: b2
rprodMap ra rb (a :*: b) = selMap ra a :*: selMap rb b

rtypeMap :: EP b r -> EP d a -> Map r a -> b -> d
rtypeMap ep1 ep2 ra = to ep2 . selMap ra . from ep1

instance Generic2 Map where
  rint2                = Map $ id
  rinteger2            = Map $ id
  rfloat2              = Map $ id
  rdouble2             = Map $ id
  rchar2               = Map $ id
  runit2               = Map $ id
  rsum2          ra rb = Map $ rsumMap ra rb
  rprod2         ra rb = Map $ rprodMap ra rb
  rtype2 ep1 ep2 ra    = Map $ rtypeMap ep1 ep2 ra

-----------------------------------------------------------------------------
-- Exported functions
-----------------------------------------------------------------------------

-- | Apply a function to all elements of a container datatype (kind @* -> *@).
map :: (FRep2 Map f) => (a -> b) -> f a -> f b
map = selMap . frep2 . Map

-- | Given a datatype @F a b@, @bimap f g@ applies the function @f :: a -> c@ to
-- every @a@-element and the function @g :: b -> d@ to every @b@-element. The
-- result is a value with transformed elements: @F c d@.
bimap :: (BiFRep2 Map f) => (a -> c) -> (b -> d) -> f a b -> f c d
bimap f g = selMap (bifrep2 (Map f) (Map g))

-- | Cast a value of one type into a value of another. This is a configurable
-- function that allows you to define your own type-safe conversions for a
-- variety of types.
--
-- @cast@ works with instances of @'Rep' ('Map' i) o@ in which you choose the
-- input type @i@ and the output type @o@ and implement the function of type @i
-- -> o@.
--
-- Here are some examples of instances (and flags you will need or want):
--
-- >   {-# LANGUAGE MultiParamTypeClasses  #-}
-- >   {-# LANGUAGE FlexibleContexts       #-}
-- >   {-# LANGUAGE FlexibleInstances      #-}
-- >   {-# OPTIONS_GHC -fno-warn-orphans   #-}
--
-- @
--   instance 'Rep' ('Map' 'Int') 'Char' where
--     'rep' = 'Map' 'chr'
-- @
--
-- @
--   instance 'Rep' ('Map' 'Float') 'Double' where
--     'rep' = 'Map' 'realToFrac'
-- @
--
-- @
--   instance 'Rep' ('Map' 'Integer') 'Integer' where
--     'rep' = 'Map' (+42)
-- @
--
-- There are no pre-defined instances, and a call to @cast@ will not compile if
-- no instances for the input and output type pair are found, so you must define
-- instances in order to use @cast@.
cast :: (Rep (Map a) b) => a -> b
cast = selMap rep

