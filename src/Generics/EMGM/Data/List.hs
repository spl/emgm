{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# OPTIONS -fno-warn-orphans       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Data.List
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic representation and instances for lists.
--
-- The main purpose of this module is to export the instances for the
-- representation dispatchers 'Rep', 'FRep', 'FRep2', and 'FRep3'. For the rare
-- cases in which it is needed, this module also exports the
-- embedding-projection pair and constructor description.
-----------------------------------------------------------------------------

module Generics.EMGM.Data.List (
  epList,
  conNil,
  conCons,
) where

import Generics.EMGM.Common
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

-- | Embedding-projection pair for lists
epList :: EP [a] (Unit :+: (a :*: [a]))
epList = EP fromList toList

-----------------------------------------------------------------------------
-- Representation values
-----------------------------------------------------------------------------

-- | Constructor description for ''nil'': @[]@
conNil :: ConDescr
conNil = ConDescr "[]" 0 [] Nonfix

-- | Constructor description for ''cons'': @(:)@
conCons :: ConDescr
conCons = ConDescr ":" 2 [] (Infixr 5)

-- | Representation for lists in 'Generic'
rList :: (Generic g) => g a -> g [a]
rList ra =
  rtype epList
    (rcon conNil runit `rsum` rcon conCons (ra `rprod` rList ra))

-- | Representation for lists in 'Generic2'
rList2 :: (Generic2 g) => g a b -> g [a] [b]
rList2 ra =
  rtype2 epList epList
    (rcon2 conNil runit2 `rsum2` rcon2 conCons (ra `rprod2` rList2 ra))

-- | Representation for lists in 'Generic3'
rList3 :: (Generic3 g) => g a b c -> g [a] [b] [c]
rList3 ra =
  rtype3 epList epList epList
    (rcon3 conNil runit3 `rsum3` rcon3 conCons (ra `rprod3` rList3 ra))

-----------------------------------------------------------------------------
-- Instance declarations
-----------------------------------------------------------------------------

instance (Generic g, Rep g a) => Rep g [a] where
  rep = rList rep

instance (Generic g) => FRep g [] where
  frep = rList

instance (Generic2 g) => FRep2 g [] where
  frep2 = rList2

instance (Generic3 g) => FRep3 g [] where
  frep3 = rList3

instance Rep (Collect [a]) [a] where
  rep = Collect (:[])

instance Rep (Everywhere [a]) [a] where
  rep = Everywhere app
    where
      app f x =
        case x of
          []   -> f []
          a:as -> f (a : selEverywhere rep f as)

instance Rep (Everywhere' [a]) [a] where
  rep = Everywhere' ($)

