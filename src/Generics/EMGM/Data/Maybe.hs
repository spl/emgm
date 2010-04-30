{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# OPTIONS -fno-warn-orphans       #-}
{-  OPTIONS -ddump-splices           -}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Data.Maybe
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic representation and instances for 'Maybe'.
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

import Generics.EMGM.Base
import Generics.EMGM.Functions.Collect
import Generics.EMGM.Functions.Everywhere

-----------------------------------------------------------------------------
-- Embedding-projection pair
-----------------------------------------------------------------------------

fromMaybe :: Maybe a -> Unit :+: a
fromMaybe Nothing   =  L Unit
fromMaybe (Just a)  =  R a

toMaybe :: Unit :+: a -> Maybe a
toMaybe (L Unit)  =  Nothing
toMaybe (R a)     =  Just a

-- | Embedding-projection pair for 'Maybe'.
epMaybe :: EP (Maybe a) (Unit :+: a)
epMaybe = EP fromMaybe toMaybe

-----------------------------------------------------------------------------
-- Representation values
-----------------------------------------------------------------------------

-- | Constructor description for 'Nothing'.
conNothing :: ConDescr
conNothing = ConDescr "Nothing" 0 [] Nonfix

-- | Constructor description for 'Just'.
conJust :: ConDescr
conJust = ConDescr "Just" 1 [] Nonfix

-- | Representation of 'Maybe' for 'frep'.
frepMaybe :: (Generic g) => g a -> g (Maybe a)
frepMaybe ra =
  rtype
    epMaybe
    (rcon conNothing runit `rsum` rcon conJust ra)

-- | Representation of 'Maybe' for 'rep'.
repMaybe :: (Generic g, Rep g a) => g (Maybe a)
repMaybe =
  rtype
    epMaybe
    (rcon conNothing rep `rsum` rcon conJust rep)

-- | Representation of 'Maybe' for 'frep2'.
frep2Maybe :: (Generic2 g) => g a b -> g (Maybe a) (Maybe b)
frep2Maybe ra =
  rtype2
    epMaybe epMaybe
    (rcon2 conNothing runit2 `rsum2` rcon2 conJust ra)

-- | Representation of 'Maybe' for 'bifrep2'.
bifrep2Maybe :: (Generic2 g) => g a b -> g (Maybe a) (Maybe b)
bifrep2Maybe =
  frep2Maybe

-- | Representation of 'Maybe' for 'frep3'.
frep3Maybe :: (Generic3 g) => g a b c -> g (Maybe a) (Maybe b) (Maybe c)
frep3Maybe ra =
  rtype3
    epMaybe epMaybe epMaybe
    (rcon3 conNothing runit3 `rsum3` rcon3 conJust ra)

-----------------------------------------------------------------------------
-- Instance declarations
-----------------------------------------------------------------------------

instance (Generic g, Rep g a) => Rep g (Maybe a) where
  rep = repMaybe

instance (Generic g) => FRep g Maybe where
  frep = frepMaybe

instance (Generic2 g) => FRep2 g Maybe where
  frep2 = frep2Maybe

instance (Generic3 g) => FRep3 g Maybe where
  frep3 = frep3Maybe

instance Rep (Collect (Maybe a)) (Maybe a) where
  rep = Collect (:[])

instance (Rep (Everywhere (Maybe a)) a) => Rep (Everywhere (Maybe a)) (Maybe a) where
  rep = Everywhere app
    where
      app f x =
        case x of
          Nothing -> f Nothing
          Just v1 -> f (Just (selEverywhere rep f v1))

instance Rep (Everywhere' (Maybe a)) (Maybe a) where
  rep = Everywhere' ($)

