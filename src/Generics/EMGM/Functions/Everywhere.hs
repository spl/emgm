{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverlappingInstances       #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Functions.Everywhere
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic function that applies a transformation everywhere in a
-- top-down manner.
--
--------------------------------------------------------------------------------

module Generics.EMGM.Functions.Everywhere (
  Everywhere(..),
  everywhere,
) where

import Generics.EMGM.Common.Base
import Generics.EMGM.Common.Representation

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | The type of a generic function that takes a function of one type, a value
-- of another type, and returns a value of the value type.
--
-- For datatypes to work with Everywhere, a special instance must be given. This
-- instance is trivial to write. Given a type @T@, the 'Rep' instance looks like
-- this:
--
-- >  {-# LANGUAGE OverlappingInstances #-}
-- >
-- >  data T = ...
-- >
-- >  instance Rep (Everywhere T) T where
-- >    rep = Everywhere (\f x -> f x)
--
-- (Note the requirement of overlapping instances.) This instance is triggered
-- when the function type (the first @T@) matches some value type (the second
-- @T@) contained within the argument to 'everywhere'. See the source of this
-- module for more examples.
newtype Everywhere a b = Everywhere { selEverywhere :: (a -> a) -> b -> b }

--------------------------------------------------------------------------------
-- Generic instance declaration
--------------------------------------------------------------------------------

rconstantCMap :: (a -> a) -> b -> b
rconstantCMap _ = id

rsumCMap :: Everywhere a b1 -> Everywhere a b2 -> (a -> a) -> (b1 :+: b2) -> b1 :+: b2
rsumCMap ra _  f (L a) = L (selEverywhere ra f a)
rsumCMap _  rb f (R b) = R (selEverywhere rb f b)

rprodCMap :: Everywhere a b1 -> Everywhere a b2 -> (a -> a) -> (b1 :*: b2) -> b1 :*: b2
rprodCMap ra rb f (a :*: b) = selEverywhere ra f a :*: selEverywhere rb f b

rtypeCMap :: EP d b -> Everywhere a b -> (a -> a) -> d -> d
rtypeCMap ep ra f = to ep . selEverywhere ra f . from ep

instance Generic (Everywhere a) where
  rconstant      = Everywhere rconstantCMap
  rsum     ra rb = Everywhere (rsumCMap ra rb)
  rprod    ra rb = Everywhere (rprodCMap ra rb)
  rtype ep ra    = Everywhere (rtypeCMap ep ra)

--------------------------------------------------------------------------------
-- Rep instance declarations
--------------------------------------------------------------------------------

instance Rep (Everywhere Int) Int where
  rep = Everywhere (\f x -> f x)

instance Rep (Everywhere Integer) Integer where
  rep = Everywhere (\f x -> f x)

instance Rep (Everywhere Double) Double where
  rep = Everywhere (\f x -> f x)

instance Rep (Everywhere Float) Float where
  rep = Everywhere (\f x -> f x)

instance Rep (Everywhere Char) Char where
  rep = Everywhere (\f x -> f x)

--------------------------------------------------------------------------------
-- Exported functions
--------------------------------------------------------------------------------

-- | Apply a transformation @a -> a@ to values of type @a@ within the argument
-- of type @b@ in a top-down manner. Values that do not match @a@ are passed
-- through 'id'.
--
-- @everywhere@ works by searching the datatype @b@ for values that are the same
-- type as the function argument type @a@. Here are some examples using the same
-- value with different functions.
--
-- @
--   ghci> let x = ['Left' 1, 'Right' \'a\', 'Left' 2] :: ['Either' 'Int' 'Char']
--   ghci> everywhere (*(3::'Int')) x
--   ['Left' 3,'Right' \'a\','Left' 6]
--   ghci> let f ('Left' i) = 'Left' (i + (37::'Int')); f ('Right' c) = 'Right' ('toUpper' c)
--   ghci> everywhere f x
--   ['Left' 38,'Right' \'A\','Left' 39]
--   ghci> everywhere (\x -> x :: 'Float') x == x
--   'True'
-- @
--
-- Note the type annotations. Since numerical constants have the type @'Num' a
-- => a@, you may need to give explicit types. Also, the function @\x -> x@ has
-- type @a -> a@, but we need to give it any non-polymorphic type here. By
-- design, there is no connection that can be inferred between the value type
-- and the function type.
--
-- @everywhere@ only works if there is an instance for the return type as
-- described in the @newtype 'Everywhere'@.
everywhere :: (Rep (Everywhere a) b) => (a -> a) -> b -> b
everywhere f = selEverywhere rep f

