-----------------------------------------------------------------------------
-- |
-- Module      :  B
-- Copyright   :  (c) 2008 - 2010 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
--
-- An example type representation.
-----------------------------------------------------------------------------

-- {-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE OverlappingInstances     #-}
{-# LANGUAGE UndecidableInstances     #-}

module B where

import Prelude hiding (Read, Show)
import qualified Prelude as P (Read, Show)
import Data.Generics (Data, Typeable)

import Generics.EMGM.Base
import Generics.EMGM.Functions.Collect
import Generics.EMGM.Functions.Everywhere
import Generics.EMGM.Data.List
import Generics.EMGM.Data.Maybe
import Generics.EMGM.Data.Tuple

data B a
  = B1 (B Double)
  | B2 (Maybe a)
  | B3 (Maybe [a])
  | B4 (a,a)
  | B5 (a,a,a) (a,a,a,a) (a,a,B a,a,a) (a,a,a,a,a,a) (a,a,a,a,a,a,a)
  deriving (P.Show, P.Read, Eq, Ord, Data, Typeable)

type B' a
  {- B1 -}  =  B Double
  {- B2 -} :+: Maybe a
  {- B3 -} :+: Maybe [a]
  {- B4 -} :+: (a,a)
  {- B5 -} :+: (a,a,a) :*: (a,a,a,a) :*: (a,a,B a,a,a) :*: (a,a,a,a,a,a) :*: (a,a,a,a,a,a,a)

fromB :: B a -> B' a
fromB b = case b of
  B1 x1             -> L x1
  B2 x1             -> R (L x1)
  B3 x1             -> R (R (L x1))
  B4 x1             -> R (R (R (L x1)))
  B5 x1 x2 x3 x4 x5 -> R (R (R (R (x1 :*: x2 :*: x3 :*: x4 :*: x5))))

toB :: B' a -> B a
toB b = case b of
  L x1                                           -> B1 x1
  R (L x1)                                       -> B2 x1
  R (R (L x1))                                   -> B3 x1
  R (R (R (L x1)))                               -> B4 x1
  R (R (R (R (x1 :*: x2 :*: x3 :*: x4 :*: x5)))) -> B5 x1 x2 x3 x4 x5

epB :: EP (B a) (B' a)
epB = EP fromB toB

conB1 = ConDescr "B1" 1 False Prefix
conB2 = ConDescr "B2" 1 False Prefix
conB3 = ConDescr "B3" 1 False Prefix
conB4 = ConDescr "B4" 1 False Prefix
conB5 = ConDescr "B5" 5 False Prefix

instance (Generic g, Rep g (B Double), Rep g (Maybe a), Rep g (Maybe [a]),
          Rep g (a,a), Rep g (a,a,a), Rep g (a,a,a,a), Rep g (a,a,B a,a,a),
          Rep g (a,a,a,a,a,a), Rep g (a,a,a,a,a,a,a))
         => Rep g (B a) where
  rep = rtype epB
       $   rcon conB1 rep
    `rsum` rcon conB2 rep
    `rsum` rcon conB3 rep
    `rsum` rcon conB4 rep
    `rsum` rcon conB5 (rep `rprod` rep `rprod` rep `rprod` rep `rprod` rep)

instance (Generic g) => FRep g B where
  frep ra = rtype epB
       $   rcon conB1 (frep rdouble)
    `rsum` rcon conB2 (frepMaybe ra)
    `rsum` rcon conB3 (frep (frepList ra))
    `rsum` rcon conB4 (frepTuple2 ra ra)
    `rsum` rcon conB5 (frepTuple3 ra ra ra `rprod`
                       frepTuple4 ra ra ra ra `rprod`
                       frepTuple5 ra ra (frep ra) ra ra `rprod`
                       frepTuple6 ra ra ra ra ra ra `rprod`
                       frepTuple7 ra ra ra ra ra ra ra)

instance (Generic2 g) => FRep2 g B where
  frep2 ra = rtype2 epB epB
       $    rcon2 conB1 (frep2 rdouble2)
    `rsum2` rcon2 conB2 (frep2Maybe ra)
    `rsum2` rcon2 conB3 (frep2 (frep2List ra))
    `rsum2` rcon2 conB4 (frep2Tuple2 ra ra)
    `rsum2` rcon2 conB5 (frep2Tuple3 ra ra ra `rprod2`
                         frep2Tuple4 ra ra ra ra `rprod2`
                         frep2Tuple5 ra ra (frep2 ra) ra ra `rprod2`
                         frep2Tuple6 ra ra ra ra ra ra `rprod2`
                         frep2Tuple7 ra ra ra ra ra ra ra)

instance (Generic3 g) => FRep3 g B where
  frep3 ra = rtype3 epB epB epB
       $    rcon3 conB1 (frep3 rdouble3)
    `rsum3` rcon3 conB3 (frep3Maybe ra)
    `rsum3` rcon3 conB3 (frep3 (frep3List ra))
    `rsum3` rcon3 conB4 (frep3Tuple2 ra ra)
    `rsum3` rcon3 conB5 (frep3Tuple3 ra ra ra `rprod3`
                         frep3Tuple4 ra ra ra ra `rprod3`
                         frep3Tuple5 ra ra (frep3 ra) ra ra `rprod3`
                         frep3Tuple6 ra ra ra ra ra ra `rprod3`
                         frep3Tuple7 ra ra ra ra ra ra ra)

instance Rep (Collect (B a)) (B a) where
  rep = Collect (:[])

instance (Rep (Everywhere (B a)) a, Rep (Everywhere (B a)) (B Double),
          Rep (Everywhere (B a)) (Maybe a), Rep (Everywhere (B a)) (Maybe [a]),
          Rep (Everywhere (B a)) (a,a), Rep (Everywhere (B a)) (a,a,a),
          Rep (Everywhere (B a)) (a,a,a,a),
          Rep (Everywhere (B a)) (a,a,B a,a,a),
          Rep (Everywhere (B a)) (a,a,a,a,a,a),
          Rep (Everywhere (B a)) (a,a,a,a,a,a,a))
         => Rep (Everywhere (B a)) (B a) where
  rep = Everywhere app
    where
      app f x =
        case x of
          B1 x1             -> f (B1 (selEverywhere rep f x1))
          B2 x1             -> f (B2 (selEverywhere rep f x1))
          B3 x1             -> f (B3 (selEverywhere rep f x1))
          B4 x1             -> f (B4 (selEverywhere rep f x1))
          B5 x1 x2 x3 x4 x5 -> f (B5 (selEverywhere rep f x1) (selEverywhere rep f x2) (selEverywhere rep f x3) (selEverywhere rep f x4) (selEverywhere rep f x5))

instance Rep (Everywhere' (B a)) (B a) where
  rep = Everywhere' ($)

