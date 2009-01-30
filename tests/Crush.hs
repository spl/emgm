{-# LANGUAGE FlexibleContexts #-}

module Crush (tests) where

import Prelude as P

import Base

import Test.HUnit

import Generics.EMGM.Functions.Crush as C

-----------------------------------------------------------------------------
-- Utility functions
-----------------------------------------------------------------------------

eqf :: (Eq b) => (a -> b) -> (a -> b) -> a -> Bool
eqf f1 f2 as = f1 as == f2 as

eqpf :: (Eq b) => p -> (p -> a -> b) -> (p -> a -> b) -> a -> Bool
eqpf p f1 f2 as = f1 p as == f2 p as

fromList :: ([a] -> a) -> [a] -> Maybe a
fromList _ [] = Nothing
fromList f xs = Just (f xs)

-----------------------------------------------------------------------------
-- Tests
-----------------------------------------------------------------------------

test_crush1 =
  "crush AssocLeft (:) [] [1,2] ~> [2,1]" ~:
    crushl (:) [] [1,2] ~?= [2,1::Int]

test_crush2 =
  "crush AssocRight (:) [] [1,2] ~> [1,2]" ~:
    crush AssocRight (:) [] [1,2] ~?= [1,2::Int]

-----------------------------------------------------------------------------
-- Properties
-----------------------------------------------------------------------------

test_flattenl_reverse = "flattenl == Prelude.reverse" ~|: f
  where f :: [Float] -> Bool
        f ls = flattenl ls == reverse ls

test_flattenr = "flattenr == id" ~|: f
  where f :: [Int] -> Bool
        f ls = flattenr ls == ls

test_firstl = "firstl == Prelude.last" ~|: f
  where f :: [Int] -> Bool
        f ls = firstl ls == last' ls
        last' :: [a] -> Maybe a
        last' [] = Nothing
        last' as = Just $ last as

test_firstr = "firstr == Prelude.head" ~|: f
  where f :: [Double] -> Bool
        f ls = firstr ls == head' ls
        head' :: [a] -> Maybe a
        head' [] = Nothing
        head' as = Just $ head as

test_and = "and == Prelude.and" ~|: (eqf C.and P.and :: [Bool] -> Bool)

test_or = "or == Prelude.or" ~|: (eqf C.or P.or :: [Bool] -> Bool)

test_any = "any == Prelude.any" ~|: (eqpf (== 0) C.any P.any :: [Integer] -> Bool)

test_all = "all == Prelude.all" ~|: (eqpf f C.all P.all :: [Float] -> Bool)
  where f x = abs x > 2

test_sum = "sum == Prelude.sum" ~|: (eqf C.sum P.sum :: [Int] -> Bool)

test_product = "product == Prelude.product" ~|: (eqf C.product P.product :: [Int] -> Bool)

test_minimum = "minimum == Prelude.minimum" ~|: (eqf C.minimum (fromList P.minimum) :: [Int] -> Bool)

test_maximum = "maximum == Prelude.maximum" ~|: (eqf C.maximum (fromList P.maximum) :: [Int] -> Bool)

test_elem1 = "elem 5 [1,2,5,2]" ~: assert (C.elem 5 [1,2,5,2::Int])
test_elem2 = "elem 8 [1,2,5,2]" ~: assert (not $ C.elem 8 [1,2,5,2::Int])

test_notElem1 = "notElem 5 [1,2,5,2]" ~: assert (not $ C.notElem 5 [1,2,5,2::Int])
test_notElem2 = "notElem 8 [1,2,5,2]" ~: assert (C.notElem 8 [1,2,5,2::Int])

-----------------------------------------------------------------------------
-- Test collection
-----------------------------------------------------------------------------

tests =
  "Crush" ~:
    [ test_crush1
    , test_crush2
    , test_flattenl_reverse
    , test_flattenr
    , test_firstl
    , test_firstr
    , test_and
    , test_or
    , test_any
    , test_all
    , test_sum
    , test_product
    , test_minimum
    , test_maximum
    , test_elem1
    , test_elem2
    , test_notElem1
    , test_notElem2
    ]

