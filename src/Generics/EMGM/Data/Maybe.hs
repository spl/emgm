{-# LANGUAGE CPP                    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS -fno-warn-orphans       #-}
{-  OPTIONS -ddump-splices           -}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Data.Maybe
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic representation and instances for 'Maybe'.
--
-- The main purpose of this module is to export the instances for the
-- representation dispatchers 'Rep', 'FRep', 'FRep2', and 'FRep3'. For the rare
-- cases in which it is needed, this module also exports the
-- embedding-projection pair and constructor description.
-----------------------------------------------------------------------------

module Generics.EMGM.Data.Maybe (
  epMaybe,
  conNothing,
  conJust,
  repMaybe,
  frepMaybe,
  frep2Maybe,
  frep3Maybe,
  bifrep2Maybe,
) where

import Generics.EMGM.Common
import Generics.EMGM.Functions.Collect
import Generics.EMGM.Functions.Everywhere

#ifndef __HADDOCK__

$(derive ''Maybe)

#else
-- The following code is used by Haddock to generate documentation. It may be
-- useful to keep around for debugging TH, so don't remove it.

-----------------------------------------------------------------------------
-- Embedding-projection pair
-----------------------------------------------------------------------------

fromMaybe :: Maybe a -> Unit :+: a
fromMaybe Nothing   =  L Unit
fromMaybe (Just a)  =  R a

toMaybe :: Unit :+: a -> Maybe a
toMaybe (L Unit)  =  Nothing
toMaybe (R a)     =  Just a

-- | Embedding-projection pair for 'Maybe'
epMaybe :: EP (Maybe a) (Unit :+: a)
epMaybe = EP fromMaybe toMaybe

-----------------------------------------------------------------------------
-- Representation values
-----------------------------------------------------------------------------

-- | Constructor description for 'Nothing'
conNothing :: ConDescr
conNothing = ConDescr "Nothing" 0 [] Nonfix

-- | Constructor description for 'Just'
conJust :: ConDescr
conJust = ConDescr "Just" 1 [] Nonfix

-- | Representation for @Maybe a@ in 'Generic'
rMaybe :: (Generic g) => g a -> g (Maybe a)
rMaybe ra =
  rtype epMaybe
    (rcon conNothing runit `rsum` rcon conJust ra)

-- | Representation for @Maybe a@ in 'Generic2'
rMaybe2 :: (Generic2 g) => g a b -> g (Maybe a) (Maybe b)
rMaybe2 ra =
  rtype2 epMaybe epMaybe
    (rcon2 conNothing runit2 `rsum2` rcon2 conJust ra)

-- | Representation for @Maybe a@ in 'Generic3'
rMaybe3 :: (Generic3 g) => g a b c -> g (Maybe a) (Maybe b) (Maybe c)
rMaybe3 ra =
  rtype3 epMaybe epMaybe epMaybe
    (rcon3 conNothing runit3 `rsum3` rcon3 conJust ra)

-----------------------------------------------------------------------------
-- Instance declarations
-----------------------------------------------------------------------------

instance (Generic g, Rep g a) => Rep g (Maybe a) where
  rep = rMaybe rep

instance (Generic g) => FRep g Maybe where
  frep = rMaybe

instance (Generic2 g) => FRep2 g Maybe where
  frep2 = rMaybe2

instance (Generic3 g) => FRep3 g Maybe where
  frep3 = rMaybe3

instance Rep (Collect (Maybe a)) (Maybe a) where
  rep = Collect (:[])

instance (Rep (Everywhere (Maybe a)) a) => Rep (Everywhere (Maybe a)) (Maybe a) where
  rep = Everywhere (\f x -> f x >>= selEverywhere rep f)

instance Rep (Everywhere' (Maybe a)) (Maybe a) where
  rep = Everywhere' ($)

#endif

