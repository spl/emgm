{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# OPTIONS -fno-warn-orphans       #-}
{-  OPTIONS -ddump-splices           -}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Data.Bool
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic representation and instances for 'Bool'.
-----------------------------------------------------------------------------

module Generics.EMGM.Data.Bool (
  epBool,
  conFalse,
  conTrue,
  repBool,
  frepBool,
  frep2Bool,
  frep3Bool,
  bifrep2Bool,
) where

import Generics.EMGM.Common
import Generics.EMGM.Functions.Collect
import Generics.EMGM.Functions.Everywhere

-----------------------------------------------------------------------------
-- Embedding-projection pair
-----------------------------------------------------------------------------

fromBool :: Bool -> Unit :+: Unit
fromBool False = L Unit
fromBool True  = R Unit

toBool :: Unit :+: Unit -> Bool
toBool (L Unit) = False
toBool (R Unit) = True

-- | Embedding-projection pair for 'Bool'.
epBool :: EP Bool (Unit :+: Unit)
epBool = EP fromBool toBool

-----------------------------------------------------------------------------
-- Representation values
-----------------------------------------------------------------------------

-- | Constructor description for 'False'.
conFalse :: ConDescr
conFalse = ConDescr "False" 0 [] Nonfix

-- | Constructor description for 'True'.
conTrue :: ConDescr
conTrue = ConDescr "True" 0 [] Nonfix

-- | Representation of 'Bool' for 'rep'.
repBool :: (Generic g) => g Bool
repBool =
  rtype
    epBool
    (rcon conFalse runit `rsum` rcon conTrue runit)

-- | Representation of 'Bool' for 'frep'.
frepBool :: (Generic g) => g Bool
frepBool =
  repBool

-- | Representation of 'Bool' for 'frep2'.
frep2Bool :: (Generic2 g) => g Bool Bool
frep2Bool =
  rtype2
    epBool epBool
    (rcon2 conFalse runit2 `rsum2` rcon2 conTrue runit2)

-- | Representation of 'Bool' for 'frep3'.
frep3Bool :: (Generic3 g) => g Bool Bool Bool
frep3Bool =
  rtype3
    epBool epBool epBool
    (rcon3 conFalse runit3 `rsum3` rcon3 conTrue runit3)

-- | Representation of 'Bool' for 'bifrep2'.
bifrep2Bool :: (Generic2 g) => g Bool Bool
bifrep2Bool =
  frep2Bool

-----------------------------------------------------------------------------
-- Instance declarations
-----------------------------------------------------------------------------

instance (Generic g) => Rep g Bool where
  rep = repBool

instance Rep (Collect Bool) Bool where
  rep = Collect (:[])

instance Rep (Everywhere Bool) Bool where
  rep = Everywhere ($)

instance Rep (Everywhere' Bool) Bool where
  rep = Everywhere' ($)

