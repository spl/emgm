-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Functions.UnzipWith
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic function that applies a (non-generic) function to every
-- element in a value, splitting the element into two. The result is a pair of
-- structurally equivalent values, one with the elements from the first
-- component of the splitting function and the other with the elements from the
-- second component.
--
-- 'UnzipWith' can be seen as the dual of 'ZipWith', though it has no direct
-- @Prelude@ counterpart. Only 'unzip' has a @Prelude@ analog.
--
-- See also "Generics.EMGM.Functions.ZipWith".
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}

module Generics.EMGM.Functions.UnzipWith (
  UnzipWith(..),
  unzipWithM,
  unzipWith,
  unzip,
) where

import Prelude hiding (unzip)

import Generics.EMGM.Base

-----------------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------------

-- | The type of a generic function that takes an argument of one type and
-- returns a pair of values with two different types.
newtype UnzipWith m a b c = UnzipWith { selUnzipWith :: a -> m (b, c) }

-----------------------------------------------------------------------------
-- Generic3 instance declaration
-----------------------------------------------------------------------------

pair :: (Monad m) => a -> m (a, a)
pair x = return (x, x)

rsumUnzipWith
  :: (Monad m)
  => UnzipWith m a1 a2 a3
  -> UnzipWith m b1 b2 b3
  -> (a1 :+: b1)
  -> m (a2 :+: b2, a3 :+: b3)
rsumUnzipWith ra _  (L a) = selUnzipWith ra a >>= \(x, y) -> return (L x, L y)
rsumUnzipWith _  rb (R b) = selUnzipWith rb b >>= \(x, y) -> return (R x, R y)

rprodUnzipWith
  :: (Monad m)
  => UnzipWith m a1 a2 a3
  -> UnzipWith m b1 b2 b3
  -> (a1 :*: b1)
  -> m (a2 :*: b2, a3 :*: b3)
rprodUnzipWith ra rb (a1 :*: b1) = do
  (a2, a3) <- selUnzipWith ra a1
  (b2, b3) <- selUnzipWith rb b1
  return (a2 :*: b2, a3 :*: b3)

rtypeUnzipWith
  :: (Monad m)
  => EP b1 a1
  -> EP b2 a2
  -> EP b3 a3
  -> UnzipWith m a1 a2 a3
  -> b1
  -> m (b2, b3)
rtypeUnzipWith ep1 ep2 ep3 ra b1 = do
  (a2, a3) <- selUnzipWith ra (from ep1 b1) 
  return (to ep2 a2, to ep3 a3)

instance (Monad m) => Generic3 (UnzipWith m) where
  rint3                    = UnzipWith $ pair
  rinteger3                = UnzipWith $ pair
  rfloat3                  = UnzipWith $ pair
  rdouble3                 = UnzipWith $ pair
  rchar3                   = UnzipWith $ pair
  runit3                   = UnzipWith $ pair
  rsum3              ra rb = UnzipWith $ rsumUnzipWith ra rb
  rprod3             ra rb = UnzipWith $ rprodUnzipWith ra rb
  rtype3 ep1 ep2 ep3 ra    = UnzipWith $ rtypeUnzipWith ep1 ep2 ep3 ra

-----------------------------------------------------------------------------
-- Identity Monad
-----------------------------------------------------------------------------

-- We introduce our own identity monad, so we don't have to import
-- Control.Monad.Identity and thus depend on the mtl package.

-- Since the use of 'Id' is completely obscured from the user except as a type
-- in the constraints of 'unzipWith' and 'unzip', we do not export it.

newtype Id a = Id { runId :: a }

instance Monad Id where
  return a = Id a
  m >>= k  = k (runId m)

-----------------------------------------------------------------------------
-- Exported functions
-----------------------------------------------------------------------------

-- | Splits a container into two structurally equivalent containers by applying
-- a function to every element, which splits it into two corresponding elements.
-- Fails if the spliting function fails

unzipWithM
  :: (Monad m, FRep3 (UnzipWith m) f)
  => (a -> m (b, c))  -- ^ Splitting function.
  -> f a              -- ^ Container of @a@-values.
  -> m (f b, f c)     -- ^ Pair of containers.
unzipWithM f = selUnzipWith (frep3 (UnzipWith f))

-- | A specialized version of 'unzipWithM' using the identity monad and a
-- splitting function that does not fail.

unzipWith :: (FRep3 (UnzipWith Id) f) => (a -> (b, c)) -> f a -> (f b, f c)
unzipWith f = runId . unzipWithM (\x -> Id (f x))

-- | A specialized version of 'unzipWith' for pairs. Generic version of
-- @Prelude.unzip@.

unzip :: (FRep3 (UnzipWith Id) f) => f (b, c) -> (f b, f c)
unzip = unzipWith id

