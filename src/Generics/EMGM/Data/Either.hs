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
-- Module      :  Generics.EMGM.Data.Either
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic representation and instances for 'Either'.
--
-- The main purpose of this module is to export the instances for the
-- representation dispatchers 'Rep' and 'BiFRep2'. For the rare cases in which
-- it is needed, this module also exports the embedding-projection pair and
-- constructor description.
-----------------------------------------------------------------------------

module Generics.EMGM.Data.Either (
  epEither,
  conLeft,
  conRight,
) where

import Generics.EMGM.Common
import Generics.EMGM.Functions.Collect

#ifndef __HADDOCK__

$(derive ''Either)

#else
-- The following code is used by Haddock to generate documentation. It may be
-- useful to keep around for debugging TH, so don't remove it.

-----------------------------------------------------------------------------
-- Embedding-projection pair
-----------------------------------------------------------------------------

fromEither :: Either a b -> a :+: b
fromEither (Left a)  = L a
fromEither (Right b) = R b

toEither :: a :+: b -> Either a b
toEither (L a) = Left a
toEither (R b) = Right b

-- | Embedding-projection pair for 'Either'
epEither :: EP (Either a b) (a :+: b)
epEither = EP fromEither toEither

-----------------------------------------------------------------------------
-- Representation values
-----------------------------------------------------------------------------

-- | Constructor description for 'Left'
conLeft :: ConDescr
conLeft = ConDescr "Left" 1 [] Nonfix

-- | Constructor description for 'Right'
conRight :: ConDescr
conRight = ConDescr "Right" 1 [] Nonfix

-- | Representation for @Either a b@ in 'Generic'
rEither :: (Generic g) => g a -> g b -> g (Either a b)
rEither ra rb = rtype epEither (rcon conLeft ra `rsum` rcon conRight rb)

-----------------------------------------------------------------------------
-- Instance declarations
-----------------------------------------------------------------------------

instance (Generic g, Rep g a, Rep g b) => Rep g (Either a b) where
  rep = rEither rep rep

instance (Generic2 g) => BiFRep2 g Either where
  bifrep2 ra rb =
    rtype2 epEither epEither $
      rcon2 conLeft ra `rsum2` rcon2 conRight rb

instance Rep (Collect (Either a b)) (Either a b) where
  rep = Collect (:[])

#endif

