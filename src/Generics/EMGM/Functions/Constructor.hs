--------------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Functions.Constructor
-- Copyright   :  (c) 2008 - 2010 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic functions for extracting information about constructors from
-- values.
--
-- This module contains two basic generic functions, 'constructor' and 'labels'.
-- The former produces the constructor description information, and the latter
-- produces a list of label descriptions.
--------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}

module Generics.EMGM.Functions.Constructor (

  -- * Constructor Information
  Constructor(..),
  constructor,

  -- * Label Information
  Labels(..),
  labels

) where

import Generics.EMGM.Base

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

-- | The type of a generic function that takes one value and returns an optional
-- constructor description.

newtype Constructor a = Constructor { selConstructor :: a -> Maybe ConDescr }

--------------------------------------------------------------------------------
-- Generic instance declaration
--------------------------------------------------------------------------------

rsumConstructor :: Constructor a -> Constructor b -> a :+: b -> Maybe ConDescr
rsumConstructor ra _  (L a) = selConstructor ra a
rsumConstructor _  rb (R b) = selConstructor rb b

instance Generic Constructor where
  rint            = Constructor $ const Nothing
  rinteger        = Constructor $ const Nothing
  rfloat          = Constructor $ const Nothing
  rdouble         = Constructor $ const Nothing
  rchar           = Constructor $ const Nothing
  runit           = Constructor $ const Nothing
  rsum      ra rb = Constructor $ rsumConstructor ra rb
  rprod     _  _  = Constructor $ const Nothing
  rcon   cd _     = Constructor $ const $ Just cd
  rlabel _  _     = Constructor $ const Nothing
  rtype  ep ra    = Constructor $ selConstructor ra . from ep

--------------------------------------------------------------------------------
-- Exported function
--------------------------------------------------------------------------------

-- | Returns a constructor description if the value is not a primitive or a
-- structure type. The argument is not evaluated and may be @undefined@.

constructor :: (Rep Constructor a) => a -> Maybe ConDescr
constructor = selConstructor rep

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

-- | The type of a generic function that takes a boolean to limit recursion and
-- a value and returns a list of label descriptions for that constructor.

newtype Labels a = Labels { selLabels :: Bool -> a -> [LblDescr] }

--------------------------------------------------------------------------------
-- Generic instance declaration
--------------------------------------------------------------------------------

rsumLabels :: Labels a -> Labels b -> Bool -> a :+: b -> [LblDescr]
rsumLabels ra _  down (L a) = selLabels ra down a
rsumLabels _  rb down (R b) = selLabels rb down b

rprodLabels :: Labels a -> Labels b -> Bool -> a :*: b -> [LblDescr]
rprodLabels ra rb down (a :*: b) = selLabels ra down a ++ selLabels rb down b

check :: (a -> [b]) -> Bool -> a -> [b]
check act down val = if down then act val else []

rconLabels :: ConDescr -> Labels a -> Bool -> a -> [LblDescr]
rconLabels _ ra = check $ selLabels ra False

rtypeLabels :: EP b a -> Labels a -> Bool -> b -> [LblDescr]
rtypeLabels ep ra = check $ selLabels ra True . from ep

none :: a -> b -> [c]
none _ _ = []

one :: c -> a -> b -> [c]
one c _ _ = [c]

instance Generic Labels where
  rint            = Labels $ none
  rinteger        = Labels $ none
  rfloat          = Labels $ none
  rdouble         = Labels $ none
  rchar           = Labels $ none
  runit           = Labels $ none
  rsum      ra rb = Labels $ rsumLabels ra rb
  rprod     ra rb = Labels $ rprodLabels ra rb
  rcon   cd ra    = Labels $ rconLabels cd ra
  rlabel ld _     = Labels $ one ld
  rtype  ep ra    = Labels $ rtypeLabels ep ra

--------------------------------------------------------------------------------
-- Exported function
--------------------------------------------------------------------------------

-- | Returns a list of descriptions for all labels in the head constructor. Does
-- not recurse into the children. The argument is not evaluated and may be
-- @undefined@.

labels :: (Rep Labels a) => a -> [LblDescr]
labels = selLabels rep True

