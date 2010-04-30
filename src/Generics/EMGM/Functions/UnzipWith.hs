{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}

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
-- element in a value, splitting the element into two. The result are two
-- structurally equivalent values, one with the elements from the first
-- component of the splitting function and the other with the elements from the
-- second component.
--
-- 'unzipWith' can be seen as the dual of the 'zipWith' function. It has no
-- @Prelude@ counterpart.
--
-- See also "Generics.EMGM.Functions.ZipWith".
-----------------------------------------------------------------------------

module Generics.EMGM.Functions.UnzipWith (
  UnzipWith(..),
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
newtype UnzipWith a b c = UnzipWith { selUnzipWith :: a -> (b, c) }

-----------------------------------------------------------------------------
-- Generic3 instance declaration
-----------------------------------------------------------------------------

rconstantUnzipWith :: a -> (a, a)
rconstantUnzipWith x = (x, x)

runitUnzipWith :: Unit -> (Unit, Unit)
runitUnzipWith _ = (Unit, Unit)

rsumUnzipWith ::
     UnzipWith a1 a2 a3
  -> UnzipWith b1 b2 b3
  -> a1 :+: b1
  -> (a2 :+: b2, a3 :+: b3)
rsumUnzipWith ra _  (L a1) = let (a2, a3) = selUnzipWith ra a1
                             in (L a2, L a3)
rsumUnzipWith _  rb (R b1) = let (b2, b3) = selUnzipWith rb b1
                             in (R b2, R b3)

rprodUnzipWith ::
     UnzipWith a1 a2 a3
  -> UnzipWith b1 b2 b3
  -> (a1 :*: b1)
  -> (a2 :*: b2, a3 :*: b3)
rprodUnzipWith ra rb (a1 :*: b1) = let (a2, a3) = selUnzipWith ra a1
                                       (b2, b3) = selUnzipWith rb b1
                                   in (a2 :*: b2, a3 :*: b3)

rtypeUnzipWith ::
     EP b1 a1
  -> EP b2 a2
  -> EP b3 a3
  -> UnzipWith a1 a2 a3
  -> b1
  -> (b2, b3)
rtypeUnzipWith ep1 ep2 ep3 ra b1 = let (a2, a3) = selUnzipWith ra (from ep1 b1) 
                                   in (to ep2 a2, to ep3 a3)

rconUnzipWith :: ConDescr -> UnzipWith a1 a2 a3 -> a1 -> (a2, a3)
rconUnzipWith _ = selUnzipWith

instance Generic3 UnzipWith where
  rconstant3               = UnzipWith rconstantUnzipWith
  runit3                   = UnzipWith runitUnzipWith
  rsum3              ra rb = UnzipWith (rsumUnzipWith ra rb)
  rprod3             ra rb = UnzipWith (rprodUnzipWith ra rb)
  rcon3  cd          ra    = UnzipWith (rconUnzipWith cd ra)
  rtype3 ep1 ep2 ep3 ra    = UnzipWith (rtypeUnzipWith ep1 ep2 ep3 ra)

-----------------------------------------------------------------------------
-- Exported functions
-----------------------------------------------------------------------------

-- | Splits a container into two structurally equivalent containers by applying
-- a function to every element, which splits it into two corresponding elements.
unzipWith ::
  (FRep3 UnzipWith f)
  => (a -> (b, c)) -- ^ Splitting function.
  -> f a           -- ^ Container of @a@-values.
  -> (f b, f c)    -- ^ Pair of containers.
unzipWith f = selUnzipWith (frep3 (UnzipWith f))

-- | Transforms a container of pairs into a container of first components and a
-- container of second components. This is a generic version of the @Prelude@
-- function of the same name.
unzip :: (FRep3 UnzipWith f) => f (a, b) -> (f a, f b)
unzip = unzipWith id

