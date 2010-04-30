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
-- Summary: Generic function that collects all values of a specified type from a
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

import Generics.EMGM.Base

-----------------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------------

-- | The type of a generic function that takes a value of one type and returns a
-- list of values of another type.
--
-- For datatypes to work with Collect, a special instance must be given. This
-- instance is trivial to write. Given a type @T@, the 'Rep' instance looks like
-- this:
--
-- >  {-# LANGUAGE OverlappingInstances #-}
-- >
-- >  data T = ...
-- >
-- >  instance Rep (Collect T) T where
-- >    rep = Collect (:[])
--
-- (Note the requirement of overlapping instances.) This instance triggers when
-- the result type (the first @T@) matches some value type (the second @T@)
-- contained within the argument to 'collect'. See the source of this module for
-- more examples.
newtype Collect b a = Collect { selCollect :: a -> [b] }

-----------------------------------------------------------------------------
-- Generic instance declaration
-----------------------------------------------------------------------------

rsumCollect :: Collect c a -> Collect c b -> a :+: b -> [c]
rsumCollect ra _  (L a) = selCollect ra a
rsumCollect _  rb (R b) = selCollect rb b

rprodCollect :: Collect c a -> Collect c b -> a :*: b -> [c]
rprodCollect ra rb (a :*: b) = selCollect ra a ++ selCollect rb b

rtypeCollect :: EP b a -> Collect c a -> b -> [c]
rtypeCollect ep ra b = selCollect ra (from ep b)

instance Generic (Collect b) where
  rint           = Collect $ const []
  rinteger       = Collect $ const []
  rfloat         = Collect $ const []
  rdouble        = Collect $ const []
  rchar          = Collect $ const []
  runit          = Collect $ const []
  rsum     ra rb = Collect $ rsumCollect ra rb
  rprod    ra rb = Collect $ rprodCollect ra rb
  rtype ep ra    = Collect $ rtypeCollect ep ra

-----------------------------------------------------------------------------
-- Rep instance declarations
-----------------------------------------------------------------------------

instance Rep (Collect Int) Int where
  rep = Collect (:[])

instance Rep (Collect Integer) Integer where
  rep = Collect (:[])

instance Rep (Collect Float) Float where
  rep = Collect (:[])

instance Rep (Collect Double) Double where
  rep = Collect (:[])

instance Rep (Collect Char) Char where
  rep = Collect (:[])

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
collect :: (Rep (Collect b) a) => a -> [b]
collect = selCollect rep

