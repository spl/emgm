-----------------------------------------------------------------------------
-- |
-- Module      :  Compare
-- Copyright   :  (c) 2008 - 2010 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Compare where

import Prelude hiding (Show, show, compare, min, max)
import qualified Prelude as P (Show, show, compare, min, max)
import Test.HUnit
import Data.Generics (Data)

import Generics.EMGM
import Generics.EMGM.Functions.Compare

import Base
import A

-----------------------------------------------------------------------------
-- Utility functions
-----------------------------------------------------------------------------

test_f mine theirs x y =
  P.show x ++ " `eq` (" ++ P.show y ++ " :: " ++ typeNameOf y ++ ")" ~:
    x `mine` y ~?= x `theirs` y

test_compare :: (P.Show a, Data a, Ord a, Rep Compare a) => a -> a -> Test
test_compare = test_f compare P.compare

test_eq :: (P.Show a, Data a, Eq a, Rep Compare a) => a -> a -> Test
test_eq = test_f eq (==)

test_neq :: (P.Show a, Data a, Eq a, Rep Compare a) => a -> a -> Test
test_neq = test_f neq (/=)

test_lt :: (P.Show a, Data a, Ord a, Rep Compare a) => a -> a -> Test
test_lt = test_f lt (<)

test_lteq :: (P.Show a, Data a, Ord a, Rep Compare a) => a -> a -> Test
test_lteq = test_f lteq (<=)

test_gt :: (P.Show a, Data a, Ord a, Rep Compare a) => a -> a -> Test
test_gt = test_f gt (>)

test_gteq :: (P.Show a, Data a, Ord a, Rep Compare a) => a -> a -> Test
test_gteq = test_f gteq (>=)

test_min :: (P.Show a, Data a, Ord a, Rep Compare a) => a -> a -> Test
test_min = test_f min P.min

test_max :: (P.Show a, Data a, Ord a, Rep Compare a) => a -> a -> Test
test_max = test_f max P.max

t1, t2 :: A (A Float)
t1 = A1 (A3 8.8 :^: 9.9) :<>: A4 (A4 (A2 11 (A1 (A1 22.22))) 33) 44
t2 = A1 (A3 8.8 :^: 9.9) :<>: A4 (A4 (A2 11 (A1 (A3 22.22))) 33) 44

-----------------------------------------------------------------------------
-- Test collections
-----------------------------------------------------------------------------

test_constants =
  "constants" ~:
    [ test_compare 42 (42 :: Int)
    , test_compare (-1) (100 :: Float)
    , test_compare 100 (-1 :: Double)
    , test_compare 42 (42 :: Integer)
    , test_compare '8' ('b' :: Char)
    ]

test_structure =
  "structure" ~:
    [ test_compare Nothing (Just (42 :: Int))
    , test_compare Nothing (Nothing :: Maybe Char)
    , test_compare (Just (42 :: Int)) Nothing
    , test_compare (1::Int,2::Int,3::Int,4::Int,5::Int) (1::Int,2::Int,3::Int,4::Int,5::Int)
    , test_compare (1::Int,2::Int,3::Int,4::Int,5::Int) (1::Int,2::Int,3::Int,3::Int,5::Int)
    , test_compare (1::Int,2::Int,3::Int,4::Int,5::Int) (1::Int,2::Int,3::Int,4::Int,6::Int)
    ]

test_functions =
  "functions" ~:
    [ "eq" ~:
        [ test_eq t1 t1
        , test_eq t1 t2
        ]
    , "neq" ~:
        [ test_neq t2 t2
        , test_neq t2 t1
        ]
    , "lt" ~:
        [ test_lt t2 t2
        , test_lt t2 t1
        , test_lt t1 t2
        ]
    , "gt" ~:
        [ test_gt t1 t1
        , test_gt t2 t1
        , test_gt t1 t2
        ]
    , "lteq" ~:
        [ test_lteq t1 t1
        , test_lteq t2 t1
        , test_lteq t1 t2
        ]
    , "gteq" ~:
        [ test_gteq t2 t2
        , test_gteq t2 t1
        , test_gteq t1 t2
        ]
    ]

tests =
  "Compare" ~:
    [ test_constants
    , test_structure
    , test_functions
    ]

