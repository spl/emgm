-----------------------------------------------------------------------------
-- |
-- Module      :  TTree
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-----------------------------------------------------------------------------

-- {-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE OverlappingInstances     #-}
{-# LANGUAGE UndecidableInstances     #-}

module TTree where

import Prelude hiding (Read, Show)
import qualified Prelude as P (Read, Show)
import Data.Generics (Data, Typeable)

import Generics.EMGM.Base
import Generics.EMGM.Functions.Collect
import Generics.EMGM.Functions.Everywhere

infixr 6 :^:
infixl 5 :<>:

data T a
  = L1 a
  | L2 Integer (T a)
  | L3 { unL3 :: Double }
  | L4 { unL4a :: T a, unL4b :: Int }
  | L5 { unL5a :: Char, unL5b :: T a, unL5c :: a }
  | T a :^: Float
  | (:<>:) { unL7a :: T a, unL7b :: T a }
  deriving (P.Show, P.Read, Eq, Ord, Data, Typeable)

type S a
  {- L1 -}    =  a
  {- L2 -}   :+: Integer :*: T a
  {- L3 -}   :+: Double
  {- L4 -}   :+: T a :*: Int
  {- L5 -}   :+: Char :*: T a :*: a
  {- :^: -}  :+: T a :*: Float
  {- :<>: -} :+: T a :*: T a

fromT :: T a -> S a
fromT t = case t of
  L1 x1         -> L x1
  L2 x1 x2      -> R (L (x1 :*: x2))
  L3 x1         -> R (R (L x1))
  L4 x1 x2      -> R (R (R (L (x1 :*: x2))))
  L5 x1 x2 x3   -> R (R (R (R (L (x1 :*: x2 :*: x3)))))
  x1 :^: x2     -> R (R (R (R (R (L (x1 :*: x2))))))
  x1 :<>: x2    -> R (R (R (R (R (R (x1 :*: x2))))))

toT :: S a -> T a
toT s = case s of
  L x1                                  -> L1 x1
  R (L (x1 :*: x2))                     -> L2 x1 x2
  R (R (L x1))                          -> L3 x1
  R (R (R (L (x1 :*: x2))))             -> L4 x1 x2
  R (R (R (R (L (x1 :*: x2 :*: x3)))))  -> L5 x1 x2 x3
  R (R (R (R (R (L (x1 :*: x2))))))     -> x1 :^: x2
  R (R (R (R (R (R (x1 :*: x2))))))     -> x1 :<>: x2

epT :: EP (T a) (S a)
epT = EP fromT toT

conL1 = ConDescr "L1" 1 False Prefix
conL2 = ConDescr "L2" 2 False Prefix
conL3 = ConDescr "L3" 1 True Prefix
conL4 = ConDescr "L4" 2 True Prefix
conL5 = ConDescr "L5" 3 True Prefix
conL6 = ConDescr ":^:" 2 False (Infix RightAssoc 6)
conL7 = ConDescr ":<>:" 2 True (Infix LeftAssoc 5)

lblUnL3 = LblDescr "unL3"
lblUnL4a = LblDescr "unL4a"
lblUnL4b = LblDescr "unL4b"
lblUnL5a = LblDescr "unL5a"
lblUnL5b = LblDescr "unL5b"
lblUnL5c = LblDescr "unL5c"
lblUnL7a = LblDescr "unL7a"
lblUnL7b = LblDescr "unL7b"

instance (Generic g, Rep g a, Rep g Char, Rep g Double, Rep g Float, Rep g Integer, Rep g Int) => Rep g (T a) where
  rep = rtype epT
       $   rcon conL1 rep
    `rsum` rcon conL2 (rep `rprod` rep)
    `rsum` rcon conL3 (rlabel lblUnL3 rep)
    `rsum` rcon conL4 (rlabel lblUnL4a rep `rprod` rlabel lblUnL4b rep)
    `rsum` rcon conL5 (rlabel lblUnL5a rep `rprod` rlabel lblUnL5b rep `rprod` rlabel lblUnL5c rep)
    `rsum` rcon conL6 (rep `rprod` rep)
    `rsum` rcon conL7 (rlabel lblUnL7a rep `rprod` rlabel lblUnL7b rep)

instance (Generic g) => FRep g T where
  frep ra = rtype epT
       $   rcon conL1 ra
    `rsum` rcon conL2 (rinteger `rprod` frep ra)
    `rsum` rcon conL3 (rlabel lblUnL3 rdouble)
    `rsum` rcon conL4 (rlabel lblUnL4a (frep ra) `rprod` rlabel lblUnL4b rint)
    `rsum` rcon conL5 (rlabel lblUnL5a rchar `rprod` rlabel lblUnL5b (frep ra) `rprod` rlabel lblUnL5c ra)
    `rsum` rcon conL6 (frep ra `rprod` rfloat)
    `rsum` rcon conL7 (rlabel lblUnL7a (frep ra) `rprod` rlabel lblUnL7b (frep ra))

instance (Generic2 g) => FRep2 g T where
  frep2 ra = rtype2 epT epT
       $    rcon2 conL1 ra
    `rsum2` rcon2 conL2 (rinteger2 `rprod2` frep2 ra)
    `rsum2` rcon2 conL3 (rlabel2 lblUnL3 rdouble2)
    `rsum2` rcon2 conL4 (rlabel2 lblUnL4a (frep2 ra) `rprod2` rlabel2 lblUnL4b rint2)
    `rsum2` rcon2 conL5 (rlabel2 lblUnL5a rchar2 `rprod2` rlabel2 lblUnL5b (frep2 ra) `rprod2` rlabel2 lblUnL5c ra)
    `rsum2` rcon2 conL6 (frep2 ra `rprod2` rfloat2)
    `rsum2` rcon2 conL7 (rlabel2 lblUnL7a (frep2 ra) `rprod2` rlabel2 lblUnL7b (frep2 ra))

instance (Generic3 g) => FRep3 g T where
  frep3 ra = rtype3 epT epT epT
       $    rcon3 conL1 ra
    `rsum3` rcon3 conL2 (rinteger3 `rprod3` frep3 ra)
    `rsum3` rcon3 conL3 (rlabel3 lblUnL3 rdouble3)
    `rsum3` rcon3 conL4 (rlabel3 lblUnL4a (frep3 ra) `rprod3` rlabel3 lblUnL4b rint3)
    `rsum3` rcon3 conL5 (rlabel3 lblUnL5a rchar3 `rprod3` rlabel3 lblUnL5b (frep3 ra) `rprod3` rlabel3 lblUnL5c ra)
    `rsum3` rcon3 conL6 (frep3 ra `rprod3` rfloat3)
    `rsum3` rcon3 conL7 (rlabel3 lblUnL7a (frep3 ra) `rprod3` rlabel3 lblUnL7b (frep3 ra))

instance Rep (Collect (T a)) (T a) where
  rep = Collect (:[])

instance (Rep (Everywhere (T a)) a) => Rep (Everywhere (T a)) (T a) where
  rep = Everywhere app
    where
      app f x =
        case x of
          L1 x1         -> f (L1 (selEverywhere rep f x1))
          L2 x1 x2      -> f (L2 (selEverywhere rep f x1) (selEverywhere rep f x2))
          L3 x1         -> f (L3 (selEverywhere rep f x1))
          L4 x1 x2      -> f (L4 (selEverywhere rep f x1) (selEverywhere rep f x2))
          L5 x1 x2 x3   -> f (L5 (selEverywhere rep f x1) (selEverywhere rep f x2) (selEverywhere rep f x3))
          x1 :^: x2     -> f (selEverywhere rep f x1 :^: selEverywhere rep f x2)
          x1 :<>: x2    -> f (selEverywhere rep f x1 :<>: selEverywhere rep f x2)

instance Rep (Everywhere' (T a)) (T a) where
  rep = Everywhere' ($)

v1 = L1 (5 :: Int)
v2 = L2 37 v1
v3 = L3 9999.9999 :: T Float
v4 = L4 v3 79
v5 = L5 'a' v4 5.0
v6 = v5 :^: 0.12345
v7 = v6 :<>: v6

