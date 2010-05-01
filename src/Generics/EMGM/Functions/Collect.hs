-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Functions.Collect
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic function that collects values of a specified type from a
-- generic value.
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverlappingInstances       #-}

module Generics.EMGM.Functions.Collect (
  Collect(..),
  collect,
) where

import Control.Monad (MonadPlus(..))

import Generics.EMGM.Base

-----------------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------------

-- | The type of a generic function that takes a value of one type and returns a
-- collection of values of another type.
--
-- For datatypes to work with Collect, a special instance must be given. This
-- instance is trivial to write. Given a type @T@, the 'Rep' instance looks like
-- this:
--
-- >  {-# LANGUAGE OverlappingInstances #-}
-- >
-- >  data T = ...
-- >
-- >  instance (MonadPlus m) => Rep (Collect m T) T where
-- >    rep = Collect return
--
-- (Note that overlapping instances are required.) This instance triggers when
-- the result type (the @T@ in @Collect m T@) matches the value type (the second
-- @T@) contained within the argument to 'collect'. See the source of this
-- module for more examples.

newtype (MonadPlus m) => Collect m b a = Collect { selCollect :: a -> m b }

-----------------------------------------------------------------------------
-- Generic instance declaration
-----------------------------------------------------------------------------

rsumCollect :: (MonadPlus m) => Collect m c a -> Collect m c b -> a :+: b -> m c
rsumCollect ra _  (L a) = selCollect ra a
rsumCollect _  rb (R b) = selCollect rb b

rprodCollect :: (MonadPlus m) => Collect m c a -> Collect m c b -> a :*: b -> m c
rprodCollect ra rb (a :*: b) = selCollect ra a `mplus` selCollect rb b

rtypeCollect :: (MonadPlus m) => EP b a -> Collect m c a -> b -> m c
rtypeCollect ep ra b = selCollect ra (from ep b)

instance (MonadPlus m) => Generic (Collect m b) where
  rint           = Collect $ const mzero
  rinteger       = Collect $ const mzero
  rfloat         = Collect $ const mzero
  rdouble        = Collect $ const mzero
  rchar          = Collect $ const mzero
  runit          = Collect $ const mzero
  rsum     ra rb = Collect $ rsumCollect ra rb
  rprod    ra rb = Collect $ rprodCollect ra rb
  rtype ep ra    = Collect $ rtypeCollect ep ra

-----------------------------------------------------------------------------
-- Rep instance declarations
-----------------------------------------------------------------------------

instance (MonadPlus m) => Rep (Collect m Int) Int where
  rep = Collect return

instance (MonadPlus m) => Rep (Collect m Integer) Integer where
  rep = Collect return

instance (MonadPlus m) => Rep (Collect m Float) Float where
  rep = Collect return

instance (MonadPlus m) => Rep (Collect m Double) Double where
  rep = Collect return

instance (MonadPlus m) => Rep (Collect m Char) Char where
  rep = Collect return

-----------------------------------------------------------------------------
-- Exported functions
-----------------------------------------------------------------------------

-- | Collect values of type @b@ from some value of type @a@. An empty list means
-- no values were collected. If you expected otherwise, be sure that you have an
-- instance such as @'Rep' ('Collect' B) B@ for the type @B@ that you are
-- collecting.
--
-- @collect@ works by searching a datatype for values that are the same type as
-- the return type specified. Here are some examples using the same value with
-- different return types:
--
-- @
--   ghci> let x = ['Left' 1, 'Right' \'a\', 'Left' 2] :: ['Either' 'Int' 'Char']
--   ghci> collect x :: ['Int']
--   [1,2]
--   ghci> collect x :: ['Char']
--   \"a\"
--   ghci> collect x == x
--   'True'
-- @
--
-- Note that the numerical constants have been declared 'Int' using the type
-- annotation. Since these natively have the type @'Num' a => a@, you may need
-- to give explicit types. By design, there is no connection that can be
-- inferred between the return type and the argument type.
--
-- @collect@ only works if there is an instance for the return type as described
-- in the @newtype 'Collect'@.
collect :: (MonadPlus m, Rep (Collect m b) a) => a -> m b
collect = selCollect rep

