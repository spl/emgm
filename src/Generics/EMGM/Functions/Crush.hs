-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.EMGM.Functions.Crush
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Generic functions that crush a container into an iteration over
-- its elements.
--
-- Crush is a datatype-generic operation on container types. It is a
-- generalization of folds, but it is not a catamorphism. To understand how
-- crush works, one can think of it as generating a list of all elements and
-- mapping an accumulating function over each one. With this image in mind, it
-- is evident that (unlike a catamorphism) very little information can be
-- determined about the structure of the container.
--
-- The EMGM implementation of 'crush' can not inherently know the associativity
-- of the binary operator. Consequently, associativity is left as an argument,
-- but there are variants specific to left- and right-associativity for
-- convenience.
--
-- Many standard Haskell datatypes (e.g. @[]@, @Data.Tree@) are designed such
-- that a constructor with more than one argument (i.e. a product structurally
-- represented by @(:*:)@) has the element on the left and any recursive points
-- towards the right. Due to this, the right-associative functions would
-- typically produce the expected values. See examples in the comments for
-- 'flattenr' and 'firstr'.
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}

module Generics.EMGM.Functions.Crush (

  -- * Crush functions

  Crush(..),
  Assoc(..),
  crush,
  crushl,
  crushr,

  -- * Left- and right-associative derived functions
  -- | The operation of these functions changes depending on the associativity
  -- of the binary operator.

  flatten,
  flattenl,
  flattenr,

  first,
  firstl,
  firstr,

  -- * Other derived functions
  -- | The operation of these functions is independent of the associativity of
  -- the binary operator. Many of these functions are generalizations of the
  -- 'Prelude' functions of the same name

  and,
  or,
  any,
  all,
  sum,
  product,
  minimum,
  maximum,
  elem,
  notElem,

) where

import Prelude hiding (and, or, any, all, elem, notElem, sum, product, max, min, maximum, minimum)

import Generics.EMGM.Base
import Generics.EMGM.Functions.Compare

-----------------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------------

-- | Associativity of the binary operator used for 'crush'
data Assoc = AssocLeft  -- ^ Left-associative
           | AssocRight -- ^ Right-associative

-- | The type of a generic function that takes an associativity and two
-- arguments of different types and returns a value of the type of the second.
newtype Crush b a = Crush { selCrush :: Assoc -> a -> b -> b }

-----------------------------------------------------------------------------
-- Generic instance declaration
-----------------------------------------------------------------------------

rsumCrush :: Crush d a -> Crush d b -> Assoc -> a :+: b -> d -> d
rsumCrush ra _  asc (L a) = selCrush ra asc a
rsumCrush _  rb asc (R b) = selCrush rb asc b

rprodCrush :: Crush d a -> Crush d b -> Assoc -> a :*: b -> d -> d
rprodCrush ra rb asc@AssocLeft  (a :*: b) = selCrush rb asc b . selCrush ra asc a
rprodCrush ra rb asc@AssocRight (a :*: b) = selCrush ra asc a . selCrush rb asc b

rtypeCrush :: EP b a -> Crush d a -> Assoc -> b -> d -> d
rtypeCrush ep ra asc = selCrush ra asc . from ep

instance Generic (Crush b) where
  rint           = Crush $ \_ _ -> id
  rinteger       = Crush $ \_ _ -> id
  rfloat         = Crush $ \_ _ -> id
  rdouble        = Crush $ \_ _ -> id
  rchar          = Crush $ \_ _ -> id
  runit          = Crush $ \_ _ -> id
  rsum     ra rb = Crush $ rsumCrush ra rb
  rprod    ra rb = Crush $ rprodCrush ra rb
  rtype ep ra    = Crush $ rtypeCrush ep ra

-----------------------------------------------------------------------------
-- Exported functions
-----------------------------------------------------------------------------

-- | Apply a function (@a -> b -> b@) to each element (@a@) of a container (@f
-- a@) and an accumulator value (@b@) to produce an accumulated result (@b@).
--
-- This is the most general form in which you must specify the associativity.
-- You may prefer to use 'crushr' or 'crushl'.
crush ::
  (FRep (Crush b) f)
  => Assoc         -- ^ Associativity of the binary operator (left or right).
  -> (a -> b -> b) -- ^ Binary operator on @a@-elements with an accumulator.
  -> b             -- ^ The initial @b@-value for the binary operator.
  -> f a           -- ^ Container of @a@-values.
  -> b             -- ^ The result after applying the above operator on all
                   -- @a@-values.
crush asc f z x = selCrush (frep (Crush f')) asc x z
  where f' _ = f -- necessary to skip the asc arg

-- | A right-associative variant of 'crush'.
crushr :: (FRep (Crush b) f) => (a -> b -> b) -> b -> f a -> b
crushr = crush AssocRight

-- | A left-associative variant of 'crush'.
crushl :: (FRep (Crush b) f) => (a -> b -> b) -> b -> f a -> b
crushl = crush AssocLeft

-- | Flatten the elements of a container into a list.
--
-- This is the most general form in which you must specify the associativity.
-- You may prefer to use 'flattenr' or 'flattenl'.
flatten :: (FRep (Crush [a]) f) => Assoc -> f a -> [a]
flatten asc = crush asc (:) []

-- | A right-associative variant of 'flatten'.
--
-- Note that, for a list @ls :: [a]@, @flattenr ls == ls@.
flattenr :: (FRep (Crush [a]) f) => f a -> [a]
flattenr = flatten AssocRight

-- | A left-associative variant of 'flatten'.
--
-- Note that, for a list @ls :: [a]@, @flattenl ls == reverse ls@.
flattenl :: (FRep (Crush [a]) f) => f a -> [a]
flattenl = flatten AssocLeft

-- | Extract the first element of a container. 'fail' if the container is empty.
--
-- This is the most general form in which you must specify the associativity.
-- You may prefer to use 'firstr' or 'firstl'.
first :: (Monad m, FRep (Crush [a]) f) => Assoc -> f a -> m a
first asc as = case flatten asc as of
                 []  -> fail "first: argument is empty"
                 a:_ -> return a

-- | A right-associative variant of 'first'.
--
-- Note that, for a list @ls :: [a]@, @fromJust (firstr ls) == head ls@.
firstr :: (Monad m, FRep (Crush [a]) f) => f a -> m a
firstr = first AssocRight

-- | A left-associative variant of 'first'.
--
-- Note that, for a list @ls :: [a]@, @fromJust (firstl ls) == last ls@.
firstl :: (Monad m, FRep (Crush [a]) f) => f a -> m a
firstl = first AssocLeft

-- | Determine if an element is a member of a container. This is a
-- generalization of the 'Prelude' function of the same name.
elem :: (Rep Compare a, FRep (Crush Bool) f) => a -> f a -> Bool
elem x = any (eq x)

-- | Determine if an element is not a member of a container. This is a
-- generalization of the 'Prelude' function of the same name.
notElem :: (Rep Compare a, FRep (Crush Bool) f) => a -> f a -> Bool
notElem x = all (neq x)

-- | Compute the sum of all elements in a container. This is a generalization of
-- the 'Prelude' function of the same name.
sum :: (Num a, FRep (Crush a) f) => f a -> a
sum = crushr (+) 0

-- | Compute the product of all elements in a container. This is a
-- generalization of the 'Prelude' function of the same name.
product :: (Num a, FRep (Crush a) f) => f a -> a
product = crushr (*) 1

-- | Determine the maximum element of a container. If the container is empty,
-- return 'Nothing'. This is a generalization of the 'Prelude' function of the
-- same name.
maximum :: (Rep Compare a, FRep (Crush (Maybe a)) f) => f a -> Maybe a
maximum = crushr f Nothing
  where f x Nothing  = Just x
        f x (Just y) = Just $ max x y

-- | Determine the minimum element of a container. If the container is empty,
-- return 'Nothing'. This is a generalization of the 'Prelude' function of the
-- same name.
minimum :: (Rep Compare a, FRep (Crush (Maybe a)) f) => f a -> Maybe a
minimum = crushr f Nothing
  where f x Nothing  = Just x
        f x (Just y) = Just $ min x y

-- | Compute the conjunction of all elements in a container. This is a
-- generalization of the 'Prelude' function of the same name.
and :: (FRep (Crush Bool) f) => f Bool -> Bool
and = crushr (&&) True

-- | Compute the disjunction of all elements in a container. This is a
-- generalization of the 'Prelude' function of the same name.
or :: (FRep (Crush Bool) f) => f Bool -> Bool
or = crushr (||) False

-- | Determine if any element in a container satisfies the predicate @p@. This
-- is a generalization of the 'Prelude' function of the same name.
any :: (FRep (Crush Bool) f) => (a -> Bool) -> f a -> Bool
any p = crushr (\x b -> b || p x) False

-- | Determine if all elements in a container satisfy the predicate @p@. This
-- is a generalization the 'Prelude' function of the same name.
all :: (FRep (Crush Bool) f) => (a -> Bool) -> f a -> Bool
all p = crushr (\x b -> b && p x) True

