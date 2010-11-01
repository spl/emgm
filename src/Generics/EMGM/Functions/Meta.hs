--------------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Functions.Meta
-- Copyright   :  (c) 2008 - 2010 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Functions for extracting meta-information about the representation.
--------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}

module Generics.EMGM.Functions.Meta (

  -- * Embedding-Projection Pair
  HasEP(..),

  -- * Constructor Description
  Con(..),
  conDescr,

  -- * Label Descriptions
  Lbls(..),
  lblDescrs

) where

import Generics.EMGM.Base

--------------------------------------------------------------------------------
-- HasEP class
--------------------------------------------------------------------------------

-- | A class to reveal the embedding-projection pair for a given datatype and
-- its isomorphic representation type.

class HasEP a b | a -> b where
  -- | The parameter is never evaluated, so @undefined@ is acceptable.
  epOf :: a -> EP a b

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

-- | The type of a generic function that takes one value and returns an optional
-- constructor description.

newtype Con a = Con { selConstructor :: a -> Maybe ConDescr }

--------------------------------------------------------------------------------
-- Generic instance declaration
--------------------------------------------------------------------------------

rsumConstructor :: Con a -> Con b -> a :+: b -> Maybe ConDescr
rsumConstructor ra _  (L a) = selConstructor ra a
rsumConstructor _  rb (R b) = selConstructor rb b

instance Generic Con where
  rint            = Con $ const Nothing
  rinteger        = Con $ const Nothing
  rfloat          = Con $ const Nothing
  rdouble         = Con $ const Nothing
  rchar           = Con $ const Nothing
  runit           = Con $ const Nothing
  rsum      ra rb = Con $ rsumConstructor ra rb
  rprod     _  _  = Con $ const Nothing
  rcon   cd _     = Con $ const $ Just cd
  rlabel _  _     = Con $ const Nothing
  rtype  ep ra    = Con $ selConstructor ra . from ep

--------------------------------------------------------------------------------
-- Exported function
--------------------------------------------------------------------------------

-- | Returns a constructor description if the value is not a primitive. The
-- argument is not evaluated and may be @undefined@.

conDescr :: (Rep Con a) => a -> Maybe ConDescr
conDescr = selConstructor rep

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

-- | The type of a generic function that takes a boolean to limit recursion and
-- a value and returns a list of label descriptions for that constructor.

newtype Lbls a = Lbls { selLabels :: Bool -> a -> [LblDescr] }

--------------------------------------------------------------------------------
-- Generic instance declaration
--------------------------------------------------------------------------------

rsumLabels :: Lbls a -> Lbls b -> Bool -> a :+: b -> [LblDescr]
rsumLabels ra _  down (L a) = selLabels ra down a
rsumLabels _  rb down (R b) = selLabels rb down b

rprodLabels :: Lbls a -> Lbls b -> Bool -> a :*: b -> [LblDescr]
rprodLabels ra rb down (a :*: b) = selLabels ra down a ++ selLabels rb down b

check :: (a -> [b]) -> Bool -> a -> [b]
check act down val = if down then act val else []

rconLabels :: ConDescr -> Lbls a -> Bool -> a -> [LblDescr]
rconLabels _ ra = check $ selLabels ra False

rtypeLabels :: EP b a -> Lbls a -> Bool -> b -> [LblDescr]
rtypeLabels ep ra = check $ selLabels ra True . from ep

none :: a -> b -> [c]
none _ _ = []

one :: c -> a -> b -> [c]
one c _ _ = [c]

instance Generic Lbls where
  rint            = Lbls $ none
  rinteger        = Lbls $ none
  rfloat          = Lbls $ none
  rdouble         = Lbls $ none
  rchar           = Lbls $ none
  runit           = Lbls $ none
  rsum      ra rb = Lbls $ rsumLabels ra rb
  rprod     ra rb = Lbls $ rprodLabels ra rb
  rcon   cd ra    = Lbls $ rconLabels cd ra
  rlabel ld _     = Lbls $ one ld
  rtype  ep ra    = Lbls $ rtypeLabels ep ra

--------------------------------------------------------------------------------
-- Exported function
--------------------------------------------------------------------------------

-- | Returns a list of descriptions for all labels in the head constructor. Does
-- not recurse into the children. The argument is not evaluated and may be
-- @undefined@.

lblDescrs :: (Rep Lbls a) => a -> [LblDescr]
lblDescrs = selLabels rep True

