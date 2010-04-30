{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# OPTIONS -fno-warn-orphans       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Data.List
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic representation and instances for lists.
-----------------------------------------------------------------------------

module Generics.EMGM.Data.List (
  epList,
  conNil,
  conCons,
  repList,
  frepList,
  frep2List,
  frep3List,
  bifrep2List,
) where

import Generics.EMGM.Common.Base
import Generics.EMGM.Functions.Collect
import Generics.EMGM.Functions.Everywhere

-----------------------------------------------------------------------------
-- Embedding-projection pair
-----------------------------------------------------------------------------

fromList :: [a] -> Unit :+: (a :*: [a])
fromList []        =  L Unit
fromList (a : as)  =  R (a :*: as)

toList :: Unit :+: (a :*: [a]) -> [a]
toList (L Unit)        =  []
toList (R (a :*: as))  =  a : as

-- | Embedding-projection pair for lists.
epList :: EP [a] (Unit :+: (a :*: [a]))
epList = EP fromList toList

-----------------------------------------------------------------------------
-- Representation values
-----------------------------------------------------------------------------

-- | Constructor description for ''nil'': @[]@.
conNil :: ConDescr
conNil = ConDescr "[]" 0 [] Nonfix

-- | Constructor description for ''cons'': @(:)@.
conCons :: ConDescr
conCons = ConDescr ":" 2 [] (Infixr 5)

-- | Representation of lists for 'frep'.
frepList :: (Generic g) => g a -> g [a]
frepList ra =
  rtype
    epList
    (rcon conNil runit `rsum` rcon conCons (ra `rprod` frepList ra))

-- | Representation of lists for 'rep'.
repList :: (Generic g, Rep g a, Rep g [a]) => g [a]
repList =
  rtype
    epList
    (rcon conNil runit `rsum` rcon conCons (rep `rprod` rep))

-- | Representation of lists for 'frep2'.
frep2List :: (Generic2 g) => g a b -> g [a] [b]
frep2List ra =
  rtype2
    epList epList
    (rcon2 conNil runit2 `rsum2` rcon2 conCons (ra `rprod2` frep2List ra))

-- | Representation of lists for 'bifrep2'.
bifrep2List :: (Generic2 g) => g a b -> g [a] [b]
bifrep2List =
  frep2List

-- | Representation of lists for 'frep3'.
frep3List :: (Generic3 g) => g a b c -> g [a] [b] [c]
frep3List ra =
  rtype3
    epList epList epList
    (rcon3 conNil runit3 `rsum3` rcon3 conCons (ra `rprod3` frep3List ra))

-----------------------------------------------------------------------------
-- Instance declarations
-----------------------------------------------------------------------------

instance (Generic g, Rep g a) => Rep g [a] where
  rep = repList

instance (Generic g) => FRep g [] where
  frep = frepList

instance (Generic2 g) => FRep2 g [] where
  frep2 = frep2List

instance (Generic3 g) => FRep3 g [] where
  frep3 = frep3List

instance Rep (Collect [a]) [a] where
  rep = Collect (:[])

instance (Rep (Everywhere [a]) a) => Rep (Everywhere [a]) [a] where
  rep = Everywhere app
    where
      app f x =
        case x of
          []   -> f []
          a:as -> f (selEverywhere rep f a : selEverywhere rep f as)

instance Rep (Everywhere' [a]) [a] where
  rep = Everywhere' ($)

