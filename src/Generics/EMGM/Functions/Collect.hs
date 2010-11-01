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

import Control.Applicative (Alternative, empty, pure, (<|>))

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
-- >  instance (Alternative f) => Rep (Collect f T) T where
-- >    rep = Collect pure
--
-- (Note that overlapping instances are required.) This instance triggers when
-- the result type (the @T@ in @Collect f T@) matches the value type (the second
-- @T@) contained within the argument to 'collect'. See the source of this
-- module for more examples.

newtype Collect f b a = Collect { selCollect :: a -> f b }

-----------------------------------------------------------------------------
-- Generic instance declaration
-----------------------------------------------------------------------------

rsumCollect :: Collect f c a -> Collect f c b -> a :+: b -> f c
rsumCollect ra _  (L a) = selCollect ra a
rsumCollect _  rb (R b) = selCollect rb b

rprodCollect :: (Alternative f) => Collect f c a -> Collect f c b -> a :*: b -> f c
rprodCollect ra rb (a :*: b) = selCollect ra a <|> selCollect rb b

rtypeCollect :: EP b a -> Collect f c a -> b -> f c
rtypeCollect ep ra b = selCollect ra (from ep b)

instance (Alternative f) => Generic (Collect f b) where
  rint           = Collect $ const empty
  rinteger       = Collect $ const empty
  rfloat         = Collect $ const empty
  rdouble        = Collect $ const empty
  rchar          = Collect $ const empty
  runit          = Collect $ const empty
  rsum     ra rb = Collect $ rsumCollect ra rb
  rprod    ra rb = Collect $ rprodCollect ra rb
  rtype ep ra    = Collect $ rtypeCollect ep ra

-----------------------------------------------------------------------------
-- Rep instance declarations
-----------------------------------------------------------------------------

instance (Alternative f) => Rep (Collect f Int) Int where
  rep = Collect pure

instance (Alternative f) => Rep (Collect f Integer) Integer where
  rep = Collect pure

instance (Alternative f) => Rep (Collect f Float) Float where
  rep = Collect pure

instance (Alternative f) => Rep (Collect f Double) Double where
  rep = Collect pure

instance (Alternative f) => Rep (Collect f Char) Char where
  rep = Collect pure

-----------------------------------------------------------------------------
-- Exported functions
-----------------------------------------------------------------------------

-- | Collect values of type @b@ from some value of type @a@. An 'empty' means
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
collect :: (Alternative f, Rep (Collect f b) a) => a -> f b
collect = selCollect rep

