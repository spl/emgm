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
-- Module      :  Generics.EMGM.Data.Bool
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic representation and instances for 'Bool'.
--
-- The main purpose of this module is to export the instances for the
-- representation dispatcher 'Rep'. For the rare cases in which it is needed,
-- this module also exports the embedding-projection pair and constructor
-- description.
-----------------------------------------------------------------------------

module Generics.EMGM.Data.Bool (
  epBool,
  conFalse,
  conTrue,
) where

import Generics.EMGM.Common
import Generics.EMGM.Functions.Collect

#ifndef __HADDOCK__

$(derive ''Bool)

#else
-- The following code is used by Haddock to generate documentation. It may be
-- useful to keep around for debugging TH, so don't remove it.

-----------------------------------------------------------------------------
-- Embedding-projection pair
-----------------------------------------------------------------------------

fromBool :: Bool -> Unit :+: Unit
fromBool False = L Unit
fromBool True  = R Unit

toBool :: Unit :+: Unit -> Bool
toBool (L Unit) = False
toBool (R Unit) = True

-- | Embedding-projection pair for 'Bool'
epBool :: EP Bool (Unit :+: Unit)
epBool = EP fromBool toBool

-----------------------------------------------------------------------------
-- Representation values
-----------------------------------------------------------------------------

-- | Constructor description for 'False'
conFalse :: ConDescr
conFalse = ConDescr "False" 0 [] Nonfix

-- | Constructor description for 'True'
conTrue :: ConDescr
conTrue = ConDescr "True" 0 [] Nonfix

-- | Representation for 'Bool' in 'Generic'
rBool :: (Generic g) => g Bool
rBool = rtype epBool (rcon conFalse runit `rsum` rcon conTrue runit)

-----------------------------------------------------------------------------
-- Instance declarations
-----------------------------------------------------------------------------

instance (Generic g) => Rep g Bool where
  rep = rBool

instance Rep (Collect Bool) Bool where
  rep = Collect (:[])

#endif

