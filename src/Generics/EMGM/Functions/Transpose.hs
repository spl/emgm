--------------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Functions.Transpose
-- Copyright   :  (c) 2008 - 2010 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic function thats transposes a value @f (g a)@ to @g (f a)@.
--
-- This is an interesting generic function since it uses multiple other generic
-- functions: 'Crush', 'Enum', 'Map', and 'ZipWith'. Notably, 'Map' and
-- 'ZipWith' are required for definining the sum and product cases of the
-- generic function. The others make the generic function easy to use.
--
-- NOTE: Be aware of the special case for empty values noted in the
-- documentation of 'tranpose'.
--------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}

module Generics.EMGM.Functions.Transpose (
  Transpose(..),
  transpose,
  transposeE,
) where

import Prelude hiding (map, Enum)
import Control.Monad (liftM)
import Data.Maybe (fromMaybe)

import Generics.EMGM.Base
import Generics.EMGM.Functions.Crush
import Generics.EMGM.Functions.Enum
import Generics.EMGM.Functions.Map
import Generics.EMGM.Functions.ZipWith

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | The type of a generic function that takes a generic value and non-generic
-- container and returns the container filled with other generic values.

newtype (Monad m) => Transpose m f c b a =
  Transpose { selTranspose :: a -> f c -> m (f b) }

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

lift :: (Monad m) => (a -> b -> c) -> a -> b -> m c
lift f x y = return $ f x y

replaceM :: (Monad m, FRep2 Map f) => a -> f b -> m (f a)
replaceM = lift replace

--------------------------------------------------------------------------------
-- Generic instance declaration
--------------------------------------------------------------------------------

rsumTranspose
  :: (Monad m, FRep2 Map f)
  => Transpose m f c a2 a1
  -> Transpose m f c b2 b1
  -> (a1 :+: b1)
  -> f c
  -> m (f (a2 :+: b2))
rsumTranspose ra _  (L a) = liftM (map L) . selTranspose ra a
rsumTranspose _  rb (R b) = liftM (map R) . selTranspose rb b

rprodTranspose
  :: (Monad m, FRep3 (ZipWith m) f)
  => Transpose m f c a2 a1
  -> Transpose m f c b2 b1
  -> (a1 :*: b1)
  -> f c
  -> m (f (a2 :*: b2))
rprodTranspose ra rb (a :*: b) x = do
  a' <- selTranspose ra a x
  b' <- selTranspose rb b x
  zipWithM (lift (:*:)) a' b'

rtypeTranspose
  :: (Monad m, FRep2 Map f)
  => EP b2 a2 -> EP b1 a1
  -> Transpose m f c a2 a1
  -> b1
  -> f c
  -> m (f b2)
rtypeTranspose ep1 ep2 ra b x = do
  v <- selTranspose ra (from ep2 b) x
  return (map (to ep1) v)

instance (Monad m, FRep2 Map f, FRep3 (ZipWith m) f)
         => Generic2 (Transpose m f c) where
  rint2                = Transpose $ replaceM
  rinteger2            = Transpose $ replaceM
  rfloat2              = Transpose $ replaceM
  rdouble2             = Transpose $ replaceM
  rchar2               = Transpose $ replaceM
  runit2               = Transpose $ replaceM
  rsum2          ra rb = Transpose $ rsumTranspose ra rb
  rprod2         ra rb = Transpose $ rprodTranspose ra rb
  rtype2 ep1 ep2 ra    = Transpose $ rtypeTranspose ep1 ep2 ra

--------------------------------------------------------------------------------
-- Exported functions
--------------------------------------------------------------------------------

-- | Transposes the structure of nested containers (types @f@ and @g@). 'fail'
-- if the outermost container is empty, because there is no generic way to
-- guarantee that both have unit constructors or, if they do, decide which one
-- to choose. See 'transposeE' for an alternative approach.

transpose
  :: (Monad m, FRep (Crush [g a]) f, FRep2 (Transpose m g a) f)
  => f (g a)
  -> m (g (f a))
transpose xs =
  firstr xs >>= selTranspose (frep2 (Transpose (const . return))) xs

-- | A convenient version of 'transpose' that returns the 'empty' value on
-- failure.

transposeE
  :: (Rep Enum (g (f a)), FRep (Crush [g a]) f, FRep2 (Transpose Maybe g a) f)
  => f (g a)
  -> g (f a)
transposeE = fromMaybe empty . transpose

