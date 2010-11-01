-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Data.Ratio
-- Copyright   :  (c) 2010 Antoine Latter, Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic representation and instances for 'Ratio'.
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# OPTIONS -fno-warn-orphans       #-}

module Generics.EMGM.Data.Ratio (
  RatioS,
  conRatio,
  repRatio,
  frepRatio,
  frep2Ratio,
  frep3Ratio,
  bifrep2Ratio,
) where

import Data.Ratio (Ratio, (%), numerator, denominator)
import Control.Applicative (Alternative, pure)
import Generics.EMGM.Base
import Generics.EMGM.Functions.Collect
import Generics.EMGM.Functions.Everywhere
import Generics.EMGM.Functions.Meta

-----------------------------------------------------------------------------
-- Embedding-projection pair
-----------------------------------------------------------------------------

type RatioS a = a :*: a

epRatio :: (Integral a) => EP (Ratio a) (a :*: a)
epRatio = EP f t
  where
    f r = numerator r :*: denominator r
    t (num :*: det) = num % det

instance (Integral a) => HasEP (Ratio a) (RatioS a) where
  epOf _ = epRatio

-----------------------------------------------------------------------------
-- Representation values
-----------------------------------------------------------------------------

-- | Constructor description for '%'.
conRatio :: ConDescr
conRatio = ConDescr "%" 2 False (Infix LeftAssoc 7)

-- | Representation of 'Ratio' for 'rep'.
repRatio :: (Integral a, Generic g, Rep g a) => g (Ratio a)
repRatio = rtype epRatio (rcon conRatio (rep `rprod` rep))

-- | Representation of 'Ratio' for 'frep'.
frepRatio :: (Integral a, Generic g) => g a -> g (Ratio a)
frepRatio a = rtype epRatio (rcon conRatio (a `rprod` a))

-- | Representation of 'Ratio' for 'frep2'.
frep2Ratio
  :: (Integral a1, Integral a2, Generic2 g)
  => g a1 a2 -> g (Ratio a1) (Ratio a2)
frep2Ratio a = rtype2 epRatio epRatio (rcon2 conRatio (a `rprod2` a))

-- | Representation of 'Ratio' for 'frep3'.
frep3Ratio
  :: (Integral a1, Integral a2, Integral a3, Generic3 g)
  => g a1 a2 a3 -> g (Ratio a1) (Ratio a2) (Ratio a3)
frep3Ratio a = rtype3 epRatio epRatio epRatio (rcon3 conRatio (a `rprod3` a))

-- | Representation of 'Ratio' for 'bifrep2'.
bifrep2Ratio
  :: (Integral a1, Integral a2, Generic2 g)
  => g a1 a2 -> g (Ratio a1) (Ratio a2)
bifrep2Ratio a = rtype2 epRatio epRatio (rcon2 conRatio (a `rprod2` a))

-----------------------------------------------------------------------------
-- Instance declarations
-----------------------------------------------------------------------------

instance (Integral a, Generic g, Rep g a) => Rep g (Ratio a) where
  rep = repRatio

instance (Alternative f) => Rep (Collect f (Ratio a)) (Ratio a) where
  rep = Collect pure

instance (Integral a, Rep (Everywhere (Ratio a)) a)
         => Rep (Everywhere (Ratio a)) (Ratio a) where
  rep = Everywhere app
    where
      app f r = f $
        selEverywhere rep f (numerator r) % selEverywhere rep f (denominator r)

instance Rep (Everywhere' (Ratio a)) (Ratio a) where
  rep = Everywhere' ($)

