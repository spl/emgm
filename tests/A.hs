-----------------------------------------------------------------------------
-- |
-- Module      :  A
-- Copyright   :  (c) 2008 - 2010 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
--
-- An example type representation.
-----------------------------------------------------------------------------

-- {-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE TypeSynonymInstances     #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE OverlappingInstances     #-}
{-# LANGUAGE UndecidableInstances     #-}

module A where

import Prelude hiding (Read, Show)
import qualified Prelude as P (Read, Show)
import Data.Generics (Data, Typeable)
import Control.Applicative (Alternative, pure)

import Generics.EMGM.Base
import Generics.EMGM.Functions.Collect
import Generics.EMGM.Functions.Everywhere
import Generics.EMGM.Functions.Meta

data A a
  = A1 a
  | A2 Integer (A a)
  | A3 { unA3 :: Double }
  | A4 { unA4a :: A a, unA4b :: Int }
  | A5 { unA5a :: Char, unA5b :: A a, unA5c :: a }
  | A a :^: Float
  | (:<>:) { unA7a :: A a, unA7b :: A a }
  deriving (P.Show, P.Read, Eq, Ord, Data, Typeable)

infixr 6 :^:
infixl 5 :<>:

type AS a
  {- A1 -}    =  a
  {- A2 -}   :+: Integer :*: A a
  {- A3 -}   :+: Double
  {- A4 -}   :+: A a :*: Int
  {- A5 -}   :+: Char :*: A a :*: a
  {- :^: -}  :+: A a :*: Float
  {- :<>: -} :+: A a :*: A a

fromA :: A a -> AS a
fromA t = case t of
  A1 x1         -> L x1
  A2 x1 x2      -> R (L (x1 :*: x2))
  A3 x1         -> R (R (L x1))
  A4 x1 x2      -> R (R (R (L (x1 :*: x2))))
  A5 x1 x2 x3   -> R (R (R (R (L (x1 :*: x2 :*: x3)))))
  x1 :^: x2     -> R (R (R (R (R (L (x1 :*: x2))))))
  x1 :<>: x2    -> R (R (R (R (R (R (x1 :*: x2))))))

toA :: AS a -> A a
toA s = case s of
  L x1                                  -> A1 x1
  R (L (x1 :*: x2))                     -> A2 x1 x2
  R (R (L x1))                          -> A3 x1
  R (R (R (L (x1 :*: x2))))             -> A4 x1 x2
  R (R (R (R (L (x1 :*: x2 :*: x3)))))  -> A5 x1 x2 x3
  R (R (R (R (R (L (x1 :*: x2))))))     -> x1 :^: x2
  R (R (R (R (R (R (x1 :*: x2))))))     -> x1 :<>: x2

epA :: EP (A a) (AS a)
epA = EP fromA toA

instance HasEP (A a) (AS a) where
  epOf _ = epA

conA1 = ConDescr "A1" 1 False Prefix
conA2 = ConDescr "A2" 2 False Prefix
conA3 = ConDescr "A3" 1 True Prefix
conA4 = ConDescr "A4" 2 True Prefix
conA5 = ConDescr "A5" 3 True Prefix
conA6 = ConDescr ":^:" 2 False (Infix RightAssoc 6)
conA7 = ConDescr ":<>:" 2 True (Infix LeftAssoc 5)

lblUnA3 = LblDescr "unA3"
lblUnA4a = LblDescr "unA4a"
lblUnA4b = LblDescr "unA4b"
lblUnA5a = LblDescr "unA5a"
lblUnA5b = LblDescr "unA5b"
lblUnA5c = LblDescr "unA5c"
lblUnA7a = LblDescr "unA7a"
lblUnA7b = LblDescr "unA7b"

instance (Generic g, Rep g a, Rep g Char, Rep g Double, Rep g Float, Rep g Integer, Rep g Int) => Rep g (A a) where
  rep = rtype epA
       $   rcon conA1 rep
    `rsum` rcon conA2 (rep `rprod` rep)
    `rsum` rcon conA3 (rlbl lblUnA3 rep)
    `rsum` rcon conA4 (rlbl lblUnA4a rep `rprod` rlbl lblUnA4b rep)
    `rsum` rcon conA5 (rlbl lblUnA5a rep `rprod` rlbl lblUnA5b rep `rprod` rlbl lblUnA5c rep)
    `rsum` rcon conA6 (rep `rprod` rep)
    `rsum` rcon conA7 (rlbl lblUnA7a rep `rprod` rlbl lblUnA7b rep)

instance (Generic g) => FRep g A where
  frep ra = rtype epA
       $   rcon conA1 ra
    `rsum` rcon conA2 (rinteger `rprod` frep ra)
    `rsum` rcon conA3 (rlbl lblUnA3 rdouble)
    `rsum` rcon conA4 (rlbl lblUnA4a (frep ra) `rprod` rlbl lblUnA4b rint)
    `rsum` rcon conA5 (rlbl lblUnA5a rchar `rprod` rlbl lblUnA5b (frep ra) `rprod` rlbl lblUnA5c ra)
    `rsum` rcon conA6 (frep ra `rprod` rfloat)
    `rsum` rcon conA7 (rlbl lblUnA7a (frep ra) `rprod` rlbl lblUnA7b (frep ra))

instance (Generic2 g) => FRep2 g A where
  frep2 ra = rtype2 epA epA
       $    rcon2 conA1 ra
    `rsum2` rcon2 conA2 (rinteger2 `rprod2` frep2 ra)
    `rsum2` rcon2 conA3 (rlbl2 lblUnA3 rdouble2)
    `rsum2` rcon2 conA4 (rlbl2 lblUnA4a (frep2 ra) `rprod2` rlbl2 lblUnA4b rint2)
    `rsum2` rcon2 conA5 (rlbl2 lblUnA5a rchar2 `rprod2` rlbl2 lblUnA5b (frep2 ra) `rprod2` rlbl2 lblUnA5c ra)
    `rsum2` rcon2 conA6 (frep2 ra `rprod2` rfloat2)
    `rsum2` rcon2 conA7 (rlbl2 lblUnA7a (frep2 ra) `rprod2` rlbl2 lblUnA7b (frep2 ra))

instance (Generic3 g) => FRep3 g A where
  frep3 ra = rtype3 epA epA epA
       $    rcon3 conA1 ra
    `rsum3` rcon3 conA2 (rinteger3 `rprod3` frep3 ra)
    `rsum3` rcon3 conA3 (rlbl3 lblUnA3 rdouble3)
    `rsum3` rcon3 conA4 (rlbl3 lblUnA4a (frep3 ra) `rprod3` rlbl3 lblUnA4b rint3)
    `rsum3` rcon3 conA5 (rlbl3 lblUnA5a rchar3 `rprod3` rlbl3 lblUnA5b (frep3 ra) `rprod3` rlbl3 lblUnA5c ra)
    `rsum3` rcon3 conA6 (frep3 ra `rprod3` rfloat3)
    `rsum3` rcon3 conA7 (rlbl3 lblUnA7a (frep3 ra) `rprod3` rlbl3 lblUnA7b (frep3 ra))

instance (Alternative f) => Rep (Collect f (A a)) (A a) where
  rep = Collect pure

instance (Rep (Everywhere (A a)) a) => Rep (Everywhere (A a)) (A a) where
  rep = Everywhere app
    where
      app f x =
        case x of
          A1 x1         -> f (A1 (selEverywhere rep f x1))
          A2 x1 x2      -> f (A2 (selEverywhere rep f x1) (selEverywhere rep f x2))
          A3 x1         -> f (A3 (selEverywhere rep f x1))
          A4 x1 x2      -> f (A4 (selEverywhere rep f x1) (selEverywhere rep f x2))
          A5 x1 x2 x3   -> f (A5 (selEverywhere rep f x1) (selEverywhere rep f x2) (selEverywhere rep f x3))
          x1 :^: x2     -> f (selEverywhere rep f x1 :^: selEverywhere rep f x2)
          x1 :<>: x2    -> f (selEverywhere rep f x1 :<>: selEverywhere rep f x2)

instance Rep (Everywhere' (A a)) (A a) where
  rep = Everywhere' ($)

v1 = A1 (5 :: Int)
v2 = A2 37 v1
v3 = A3 9999.9999 :: A Float
v4 = A4 v3 79
v5 = A5 'a' v4 5.0
v6 = v5 :^: 0.12345
v7 = v6 :<>: v6

