{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Functions.Enum
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic function that enumerates the values of a datatype.
--
-- 'enum' generates a list of the values of a datatypes. It will produce all
-- values of all supported datatypes (with only a few exceptions [1]). For
-- datatypes that have an infinite enumeration (e.g. 'Integer' and @[a]@),
-- 'enum' produces an infinite list.
--
-- A number of the techniques used to write 'enum' came from a talk by Mark
-- Jones at the 2008 Advanced Functional Programming Summer School. The authors
-- gratefully acknowledge his contribution.
--
-- [1] The exceptions are 'Float' and 'Double'. These are treated in the same way
-- as their 'Enum' instances are treated. The result looks like this:
-- @[0.0,-1.0,1.0,-2.0,..]@, thus skipping all non-integral values. Note that
-- these may overflow, because they are unbounded.
-----------------------------------------------------------------------------

module Generics.EMGM.Functions.Enum (
  Enum(..),
  enum,
  enumN,
  empty,
) where

import Prelude hiding (Enum)
import qualified Prelude as P (Enum)
import Data.List (genericTake)

import Generics.EMGM.Common

-----------------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------------

-- | The type of a generic function that takes no arguments and returns a list
-- of some type.
newtype Enum a = Enum { selEnum :: [a] }

-----------------------------------------------------------------------------
-- Utility functions
-----------------------------------------------------------------------------

infixr 5 |||

-- | Interleave elements from two lists. Similar to (++), but swap left and
-- right arguments on every recursive application.
--
-- From Mark Jones' talk at AFP2008
(|||) :: [a] -> [a] -> [a]
[]     ||| ys = ys
(x:xs) ||| ys = x : ys ||| xs

-- | Diagonalization of nested lists. Ensure that some elements from every
-- sublist will be included. Handles infinite sublists.
--
-- From Mark Jones' talk at AFP2008
diag :: [[a]] -> [a]
diag = concat . foldr skew [] . map (map (\x -> [x]))

skew :: [[a]] -> [[a]] -> [[a]]
skew []     ys = ys
skew (x:xs) ys = x : combine (++) xs ys

combine :: (a -> a -> a) -> [a] -> [a] -> [a]
combine _ xs     []     = xs
combine _ []     ys     = ys
combine f (x:xs) (y:ys) = f x y : combine f xs ys

infixr 6 ><

-- | Cartesian product with diagonalization.
--
-- From Mark Jones' talk at AFP2008
(><) :: [a] -> [b] -> [a :*: b]
xs >< ys = diag [ [ x :*: y | y <- ys ] | x <- xs ]

-----------------------------------------------------------------------------
-- Generic instance declaration
-----------------------------------------------------------------------------

-- | 'Int' values starting with 0 and enumerating to the positive and negative
-- bounds.
intEnum :: [Int]
intEnum = [0..maxBound] ||| pred 0 `downTo` minBound
  where downTo :: Int -> Int -> [Int]
        downTo n m | n < m = []
        downTo n m         = n : downTo (n - 1) m

-- | Numeric and enumerable values starting with 0 and enumerating in the
-- positive and negative directions. Useful for 'Integer', 'Float', and
-- 'Double'.
numEnum :: (Num a, P.Enum a) => [a]
numEnum = [0..] ||| map negate [1..]

rsumEnum :: Enum a -> Enum b -> [a :+: b]
rsumEnum ra rb = [L x | x <- selEnum ra]
                 ||| [R y | y <- selEnum rb]

rprodEnum :: Enum a -> Enum b -> [a :*: b]
rprodEnum ra rb = selEnum ra >< selEnum rb

rtypeEnum :: EP b a -> Enum a -> [b]
rtypeEnum ep ra = map (to ep) $ selEnum ra

instance Generic Enum where
  rconstant      = error "Unreachable"
  rint           = Enum intEnum
  rinteger       = Enum numEnum
  rfloat         = Enum numEnum
  rdouble        = Enum numEnum
  rchar          = Enum [minBound..maxBound]
  runit          = Enum [Unit]
  rsum     ra rb = Enum (rsumEnum ra rb)
  rprod    ra rb = Enum (rprodEnum ra rb)
  rtype ep ra    = Enum (rtypeEnum ep ra)

-----------------------------------------------------------------------------
-- Exported functions
-----------------------------------------------------------------------------

-- | Enumerate the values of a datatype. If the number of values is infinite,
-- the result will be an infinite list. The remaining functions are derived from
-- 'enum'.
enum :: (Rep Enum a) => [a]
enum = selEnum rep

-- | Enumerate the first @n@ values of a datatype. This is a shortcut for
-- @'genericTake' n ('enum')@.
enumN :: (Integral n, Rep Enum a) => n -> [a]
enumN n = genericTake n $ enum

-- | Returns the first element of the enumeration from 'enum'. This is
-- often called the neutral or empty value.
empty :: (Rep Enum a) => a
empty = head $ enumN (1::Int)

