-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Functions.ZipWith
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic function that applies a (non-generic) function to every
-- pair of corresponding elements in two structurally equivalent polymorphic
-- values to produce a third (also structurally equivalent) value with the
-- result of each application in every element location.
--
-- 'zipWith' is a generic version of the @Prelude@ @zipWith@ function. It works
-- on all supported container datatypes of kind @* -> *@.
--
-- The important concepts for `zipWith` are /structural equivalence/ and
-- /corresponding elements/. A regular, algebraic datatype can be visualized as
-- some sort of tree representing its structure. For 'zipWith' to be successful
-- (and not return 'Nothing'), its two container arguments must have exactly the
-- same tree shape. If the shapes of the arguments differ, then it is unclear
-- what the shape of the result is supposed to be. As a result, 'zipWith'
-- safely returns 'Nothing'.
--
-- Corresponding elements are those elements that are located in the same place
-- in the tree of each argument. If you were to traverse the tree to get to
-- element x in one tree, then its corresponding element y in the other tree
-- should require the exact same path to reach it.
--
-- See also "Generics.EMGM.Functions.UnzipWith".
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}

module Generics.EMGM.Functions.ZipWith (
  ZipWith(..),
  zipWith,
  zip,
) where

import Prelude hiding (zipWith, zip)
import Control.Monad (liftM)

import Generics.EMGM.Base

-----------------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------------

-- | The type of a generic function that takes two arguments of two different
-- types and optionally returns a value of a third type.
newtype ZipWith a b c = ZipWith { selZipWith :: a -> b -> Maybe c }

-----------------------------------------------------------------------------
-- Generic3 instance declaration
-----------------------------------------------------------------------------

check :: (Eq a) => a -> a -> Maybe a
check x y = if x == y then Just x else Nothing

rsumZipWith ::
  ZipWith a1 a2 a3
  -> ZipWith b1 b2 b3
  -> a1 :+: b1
  -> a2 :+: b2
  -> Maybe (a3 :+: b3)
rsumZipWith ra _  (L a1) (L a2) = liftM L $ selZipWith ra a1 a2
rsumZipWith _  rb (R b1) (R b2) = liftM R $ selZipWith rb b1 b2
rsumZipWith _  _  _      _      = Nothing

rprodZipWith ::
  ZipWith a1 a2 a3
  -> ZipWith b1 b2 b3
  -> (a1 :*: b1)
  -> (a2 :*: b2)
  -> Maybe (a3 :*: b3)
rprodZipWith ra rb (a1 :*: b1) (a2 :*: b2) =
  do a <- selZipWith ra a1 a2
     b <- selZipWith rb b1 b2
     return (a :*: b)

rtypeZipWith ::
  EP b1 a1
  -> EP b2 a2
  -> EP b3 a3
  -> ZipWith a1 a2 a3
  -> b1
  -> b2
  -> Maybe b3
rtypeZipWith ep1 ep2 ep3 ra b1 b2 =
  liftM (to ep3) $ selZipWith ra (from ep1 b1) (from ep2 b2)

instance Generic3 ZipWith where
  rint3                    = ZipWith $ check
  rinteger3                = ZipWith $ check
  rfloat3                  = ZipWith $ check
  rdouble3                 = ZipWith $ check
  rchar3                   = ZipWith $ check
  runit3                   = ZipWith $ check
  rsum3              ra rb = ZipWith $ rsumZipWith ra rb
  rprod3             ra rb = ZipWith $ rprodZipWith ra rb
  rtype3 ep1 ep2 ep3 ra    = ZipWith $ rtypeZipWith ep1 ep2 ep3 ra

-----------------------------------------------------------------------------
-- Exported functions
-----------------------------------------------------------------------------

-- | Combine two structurally equivalent containers into one by applying a
-- function to every corresponding pair of elements. Returns 'Nothing' if @f a@
-- and @f b@ have different shapes.
zipWith ::
  (FRep3 ZipWith f)
  => (a -> b -> c)       -- ^ Binary operator on elements of containers.
  -> f a                 -- ^ Container of @a@-values.
  -> f b                 -- ^ Container of @b@-values.
  -> Maybe (f c)         -- ^ Container of @c@-values if successful or 'Nothing'
                         -- if failed.
zipWith f = selZipWith (frep3 (ZipWith f'))
  where f' a b = Just $ f a b

-- | Combine two containers into a single container with pairs of the original
-- elements. See 'zipWith' for restrictions. This is a generic version of the
-- @Prelude@ function of the same name.
zip :: (FRep3 ZipWith f) => f a -> f b -> Maybe (f (a, b))
zip = zipWith (,)

