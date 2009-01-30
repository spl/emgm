{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE UndecidableInstances     #-}
{-  OPTIONS -ddump-splices             -}

module TTree where

import Prelude hiding (Read, Show)
import qualified Prelude as P (Read, Show)
import Data.Generics (Data, Typeable)

import Generics.EMGM

infixr 6 :^:
infixl 5 :<>:

data TTree a
  = L1 a
  | L2 a (TTree a)
  | L3 { unL3 :: a }
  | L4 { unL4t :: TTree a, unL4a :: a }
  | L5 { unL5a1 :: a, unL5t :: TTree a, unL5a2 :: a }
  | TTree a :^: a
  | (:<>:) { left :: TTree a, right :: TTree a }
  deriving (P.Show, P.Read, Eq, Ord, Data, Typeable)

$(deriveWith [(":<>:", DefinedAs "L6")] ''TTree)
conL6 = ConDescr ":<>:" 2 ["left","right"] (Infixr 5)

{-

type TTreeS a
  {- L1   -} =   a
  {- L2   -} :+: a :*: TTree a
  {- L3   -} :+: a
  {- L4   -} :+: TTree a :*: a
  {- L5   -} :+: a :*: TTree a :*: a
  {- :^:  -} :+: TTree a :*: a
  {- :<>: -} :+: TTree a :*: TTree a

fromTTree :: TTree a -> TTreeS a
fromTTree (L1 a)         = L a
fromTTree (L2 a tt)      = R (L (a :*: tt))
fromTTree (L3 a)         = R (R (L a))
fromTTree (L4 a tt)      = R (R (R (L (a :*: tt))))
fromTTree (L5 a1 tt a2)  = R (R (R (R (L (a1 :*: tt :*: a2)))))
fromTTree (tt :^: a)     = R (R (R (R (R (L (tt :*: a))))))
fromTTree (tt1 :<>: tt2) = R (R (R (R (R (R (tt1 :*: tt2))))))

toTTree :: TTreeS a -> TTree a
toTTree (L a)                                  = (L1 a)
toTTree (R (L (a :*: tt)))                     = (L2 a tt)
toTTree (R (R (L a)))                          = (L3 a)
toTTree (R (R (R (L (tt :*: a)))))             = (L4 tt a)
toTTree (R (R (R (R (L (a1 :*: tt :*: a2)))))) = (L5 a1 tt a2)
toTTree (R (R (R (R (R (L (tt :*: a)))))))     = (tt :^: a)
toTTree (R (R (R (R (R (R (tt1 :*: tt2)))))))  = (tt1 :<>: tt2)

epTTree :: EP (TTree a) (TTreeS a)
epTTree = EP fromTTree toTTree

l1, l2, l3, l4, l5, b1, b2 :: ConDescr
l1 = ConDescr "L1"   1 [] Nonfix
l2 = ConDescr "L2"   2 [] Nonfix
l3 = ConDescr "L3"   1 ["unL3"] Nonfix
l4 = ConDescr "L4"   2 ["unL4t","unL4a"] Nonfix
l5 = ConDescr "L5"   3 ["unL5a1","unL5t","unL5a2"] Nonfix
b1 = ConDescr ":^:"  2 [] (Infixr 6)
b2 = ConDescr ":<>:" 2 ["left","right"] (Infixr 5)

rTTree :: Generic g => g a -> g (TTree a)
rTTree ra = rtype epTTree $
   {- L1   -}        rcon l1 ra
   {- L2   -} `rsum` rcon l2 (ra `rprod` rTTree ra)
   {- L3   -} `rsum` rcon l3 ra
   {- L4   -} `rsum` rcon l4 (rTTree ra `rprod` ra)
   {- L5   -} `rsum` rcon l5 (ra `rprod` rTTree ra `rprod` ra)
   {- :^:  -} `rsum` rcon b1 (rTTree ra `rprod` ra)
   {- :<>: -} `rsum` rcon b2 (rTTree ra `rprod` rTTree ra)

instance (Generic g, Rep g a) => Rep g (TTree a) where
  rep = rTTree rep

instance Generic g => FRep g TTree where
  frep = rTTree

rTTree2 :: Generic2 g => g a b -> g (TTree a) (TTree b)
rTTree2 ra = rtype2 epTTree epTTree $
   {- L1   -}         rcon2 l1 ra
   {- L2   -} `rsum2` rcon2 l2 (ra `rprod2` rTTree2 ra)
   {- L3   -} `rsum2` rcon2 l3 ra
   {- L4   -} `rsum2` rcon2 l4 (rTTree2 ra `rprod2` ra)
   {- L5   -} `rsum2` rcon2 l5 (ra `rprod2` rTTree2 ra `rprod2` ra)
   {- :^:  -} `rsum2` rcon2 b1 (rTTree2 ra `rprod2` ra)
   {- :<>: -} `rsum2` rcon2 b2 (rTTree2 ra `rprod2` rTTree2 ra)

instance Generic2 g => FRep2 g TTree where
  frep2 = rTTree2

rTTree3 :: Generic3 g => g a b c -> g (TTree a) (TTree b) (TTree c)
rTTree3 ra = rtype3 epTTree epTTree epTTree $
   {- L1   -}         rcon3 l1 ra
   {- L2   -} `rsum3` rcon3 l2 (ra `rprod3` rTTree3 ra)
   {- L3   -} `rsum3` rcon3 l3 ra
   {- L4   -} `rsum3` rcon3 l4 (rTTree3 ra `rprod3` ra)
   {- L5   -} `rsum3` rcon3 l5 (ra `rprod3` rTTree3 ra `rprod3` ra)
   {- :^:  -} `rsum3` rcon3 b1 (rTTree3 ra `rprod3` ra)
   {- :<>: -} `rsum3` rcon3 b2 (rTTree3 ra `rprod3` rTTree3 ra)

instance Generic3 g => FRep3 g TTree where
  frep3 = rTTree3

-}

