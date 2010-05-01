-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Data.Either
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic representation and instances for 'Either'.
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# OPTIONS -fno-warn-orphans       #-}
{-  OPTIONS -ddump-splices           -}

module Generics.EMGM.Data.Either (
  conLeft,
  conRight,
  repEither,
  frepEither,
  frep2Either,
  frep3Either,
  bifrep2Either,
) where

import Generics.EMGM.Base
import Generics.EMGM.Functions.Collect
import Generics.EMGM.Functions.Everywhere

-----------------------------------------------------------------------------
-- Embedding-projection pair
-----------------------------------------------------------------------------

type EitherS a b = a :+: b

epEither :: EP (Either a b) (EitherS a b)
epEither = EP fromEither toEither
  where
    fromEither (Left a)  = L a
    fromEither (Right b) = R b
    toEither (L a) = Left a
    toEither (R b) = Right b

instance Representable (Either a b) (EitherS a b) where
  epOf _ = epEither

-----------------------------------------------------------------------------
-- Representation values
-----------------------------------------------------------------------------

-- | Constructor description for 'Left'.
conLeft :: ConDescr
conLeft = ConDescr "Left" 1 False Prefix

-- | Constructor description for 'Right'.
conRight :: ConDescr
conRight = ConDescr "Right" 1 False Prefix

-- | Representation of 'Either' for 'frep'.
frepEither :: (Generic g) => g a -> g b -> g (Either a b)
frepEither ra rb =
  rtype
    epEither
    (rcon conLeft ra `rsum` rcon conRight rb)

-- | Representation of 'Either' for 'rep'.
repEither :: (Generic g, Rep g a, Rep g b) => g (Either a b)
repEither =
  rtype
    epEither
    (rcon conLeft rep `rsum` rcon conRight rep)

-- | Representation of 'Either' for 'frep2'.
frep2Either :: (Generic2 g) => g a1 a2 -> g b1 b2 -> g (Either a1 b1) (Either a2 b2)
frep2Either ra rb =
  rtype2
    epEither epEither
    (rcon2 conLeft ra `rsum2` rcon2 conRight rb)

-- | Representation of 'Either' for 'bifrep2'.
bifrep2Either :: (Generic2 g) => g a1 a2 -> g b1 b2 -> g (Either a1 b1) (Either a2 b2)
bifrep2Either =
  frep2Either

-- | Representation of 'Either' for 'frep3'.
frep3Either :: (Generic3 g) => g a1 a2 a3 -> g b1 b2 b3 -> g (Either a1 b1) (Either a2 b2) (Either a3 b3)
frep3Either ra rb =
  rtype3
    epEither epEither epEither
    (rcon3 conLeft ra `rsum3` rcon3 conRight rb)

-----------------------------------------------------------------------------
-- Instance declarations
-----------------------------------------------------------------------------

instance (Generic g, Rep g a, Rep g b) => Rep g (Either a b) where
  rep = repEither

instance (Generic2 g) => BiFRep2 g Either where
  bifrep2 = bifrep2Either

instance Rep (Collect (Either a b)) (Either a b) where
  rep = Collect (:[])

instance (Rep (Everywhere (Either a b)) a, Rep (Everywhere (Either a b)) b)
         => Rep (Everywhere (Either a b)) (Either a b) where
  rep = Everywhere app
    where
      app f x =
        case x of
          Left a  -> f (Left (selEverywhere rep f a))
          Right b -> f (Right (selEverywhere rep f b))

instance Rep (Everywhere' (Either a b)) (Either a b) where
  rep = Everywhere' ($)

