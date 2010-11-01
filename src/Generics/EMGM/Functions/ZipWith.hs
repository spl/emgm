-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Functions.ZipWith
-- Copyright   :  (c) 2008 - 2010 Universiteit Utrecht
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
-- The important concepts for 'zipWithM' are /structural equivalence/ and
-- /corresponding elements/. For 'zipWithM' to be successful (and not 'fail'),
-- its two container arguments must have exactly the same shape. If the shapes
-- of the arguments differ, then it is unclear what the shape of the result is
-- supposed to be. As a result, 'zipWithM' will 'fail'.
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
  zipWithM,
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
-- types and returns a value of a third type in a Monad.
newtype ZipWith m a b c = ZipWith { selZipWith :: a -> b -> m c }

-----------------------------------------------------------------------------
-- Generic3 instance declaration
-----------------------------------------------------------------------------

check :: (Eq a, Show a, Monad m) => a -> a -> m a
check x y
  | x == y    = return x
  | otherwise = fail $ "mismatched values: '" ++ show x ++ "' /= '" ++ show y ++ "'"

rsumZipWith
  :: (Monad m)
  => ZipWith m a1 a2 a3
  -> ZipWith m b1 b2 b3
  -> a1 :+: b1
  -> a2 :+: b2
  -> m (a3 :+: b3)
rsumZipWith ra _  (L a1) (L a2) = liftM L $ selZipWith ra a1 a2
rsumZipWith _  rb (R b1) (R b2) = liftM R $ selZipWith rb b1 b2
rsumZipWith _  _  _      _      = fail "mismatched sum"

rprodZipWith
  :: (Monad m)
  => ZipWith m a1 a2 a3
  -> ZipWith m b1 b2 b3
  -> (a1 :*: b1)
  -> (a2 :*: b2)
  -> m (a3 :*: b3)
rprodZipWith ra rb (a1 :*: b1) (a2 :*: b2) =
  do a <- selZipWith ra a1 a2
     b <- selZipWith rb b1 b2
     return (a :*: b)

rtypeZipWith
  :: (Monad m)
  => EP b1 a1
  -> EP b2 a2
  -> EP b3 a3
  -> ZipWith m a1 a2 a3
  -> b1
  -> b2
  -> m b3
rtypeZipWith ep1 ep2 ep3 ra b1 b2 =
  liftM (to ep3) $ selZipWith ra (from ep1 b1) (from ep2 b2)

instance (Monad m) => Generic3 (ZipWith m) where
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
-- function to every corresponding pair of elements. Fails if (1) the binary
-- operator fails or (2) @f a@ and @f b@ have different shapes.

zipWithM
  :: (Monad m, FRep3 (ZipWith m) f)
  => (a -> b -> m c)     -- ^ Binary operator on elements of containers.
  -> f a                 -- ^ Container of @a@-values.
  -> f b                 -- ^ Container of @b@-values.
  -> m (f c)             -- ^ Container of @c@-values within a Monad @m@.
zipWithM f = selZipWith (frep3 (ZipWith f))

-- | A specialized version of 'zipWithM' for the 'Maybe' monad and a binary
-- operator that does not fail. Generic version of @Prelude.zipWith@.

zipWith :: (FRep3 (ZipWith Maybe) f) => (a -> b -> c) -> f a -> f b -> Maybe (f c)
zipWith f = zipWithM (\a b -> Just $ f a b)

-- | A specialized version of 'zipWith' for pairs. Generic version of
-- @Prelude.zip@.

zip :: (FRep3 (ZipWith Maybe) f) => f a -> f b -> Maybe (f (a, b))
zip = zipWith (,)

