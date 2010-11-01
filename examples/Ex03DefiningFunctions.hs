{-# LANGUAGE FlexibleContexts           #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Ex03DefiningFunctions
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
--
-- This example module shows how you can use the EMGM library to define your own
-- generic functions. As an example, the function which produces an empty value
-- of a datatype is defined. Several terms are successively defined and their
-- evaluation is shown in a comment after the definition.
--
-----------------------------------------------------------------------------

module Ex03DefiningFunctions where

import Generics.EMGM as G hiding (empty)

-- Defining your own generic functions

-- Empty is a simple generic producer.

-- Type for empty
newtype Empty a = Empty { selEmpty :: a }

-- Instance of Generic
instance Generic Empty where
  rint           = Empty 0
  rinteger       = Empty 0
  rfloat         = Empty 0
  rdouble        = Empty 0
  rchar          = Empty '\NUL'
  runit          = Empty Unit
  rsum     ra _  = Empty (L (selEmpty ra))
  rprod    ra rb = Empty (selEmpty ra :*: selEmpty rb)
  rcon  _  ra    = Empty (selEmpty ra)
  rtype ep ra    = Empty (to ep (selEmpty ra))

-- This is the function that actually gets used. It returns an "empty" value of
-- a datatype.
empty :: (Rep Empty a) => a
empty = selEmpty rep

-- Examples of using 'empty'

emptyDouble :: Double
emptyDouble = empty
-- Evaluates to: 0.0

emptyList :: (Rep Empty a) => [a]
emptyList = empty
-- (emptyList :: [Int]) evaluates to: []

