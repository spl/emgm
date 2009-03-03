
-----------------------------------------------------------------------------
-- |
-- Module      :  Collect
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-----------------------------------------------------------------------------

module Collect (tests) where

import Test.HUnit
import Generics.EMGM

-----------------------------------------------------------------------------
-- Utility functions
-----------------------------------------------------------------------------

test_c descr actual expected = descr ~: (collect actual) ~?= expected

-----------------------------------------------------------------------------
-- Test collection
-----------------------------------------------------------------------------

tests =
  "Collect" ~:
    [ test_c "[Maybe Int] -> [Int]" [Just 1,Nothing,Just 2::Maybe Int] [1,2::Int]
    , test_c "[Maybe Int] -> [Maybe Int]" [Just 1,Nothing,Just 2::Maybe Int] [Just 1,Nothing,Just 2::Maybe Int]
    , test_c "Maybe [String] -> [String]" (Just ["a","b"]) ["a","b"]
    , test_c "Maybe Float -> [Float]" (Just (3::Float)) [3::Float]
    , test_c "Either Integer Double -> [Integer]" (Left 98::Either Integer Double) [98::Integer]
    , test_c "Either Integer Double -> [Double]" (Left 98::Either Integer Double) ([]::[Double])
    , test_c "Either Integer Double -> [Either Integer Double]" (Left 98::Either Integer Double) [Left 98::Either Integer Double]
    , test_c "(Bool,Double) -> [Double]" (True,99::Double) [99::Double]
    , test_c "(Bool,Double,Float) -> [Bool]" (False,99::Double,0::Float) [False]
    , test_c "(Int,Double,Float,Integer) -> [Float]" (1::Int,99::Double,0::Float,2::Integer) [0::Float]
    , test_c "(Char,Char,Int,Char,Int) -> [Char]" ('a','b',5::Int,'c',9::Int) "abc"
    , test_c "(Int,[Float],Int,[Float],Int,[Float]) -> [[Float]]" (1::Int,[11::Float],5::Int,[22::Float],9::Int,[33::Float]) [[11],[22],[33::Float]]
    , test_c "(Int,Int,Int,Int,Int,Int,Int) -> [Int]" (1::Int,3::Int,5::Int,7::Int,9::Int,11::Int,13::Int) [1,3,5,7,9,11,13::Int]
    , test_c "Maybe (Int,Int,Int,Int,Int,Int,Int) -> [(Int,Int,Int,Int,Int,Int,Int)]" (Just (1::Int,3::Int,5::Int,7::Int,9::Int,11::Int,13::Int)) [(1::Int,3::Int,5::Int,7::Int,9::Int,11::Int,13::Int)]
    , test_c "[(Int,Int,Int,Int,Int,Int)] -> [(Int,Int,Int,Int,Int,Int)]" [(1::Int,3::Int,5::Int,7::Int,9::Int,11::Int)] [(1::Int,3::Int,5::Int,7::Int,9::Int,11::Int)]
    , test_c "[(Int,Int,Int,Int,Int)] -> [(Int,Int,Int,Int,Int)]" (1::Int,3::Int,5::Int,7::Int,9::Int) [(1::Int,3::Int,5::Int,7::Int,9::Int)]
    , test_c "[(Int,Double,Float,Integer)] -> [(Int,Double,Float,Integer)]" (1::Int,99::Double,0::Float,2::Integer) [(1::Int,99::Double,0::Float,2::Integer)]
    , test_c "Either (Bool,Double) (Bool,Double,Float) -> [(Bool,Double)]" (Left (True,99)::Either (Bool,Double) (Bool,Double,Float)) [(True,99::Double)]
    , test_c "Either (Bool,Double) (Bool,Double,Float) -> [(Bool,Double,Float)]" (Right (False,99,0)::Either (Bool,Double) (Bool,Double,Float)) [(False,99::Double,0::Float)]
    ]

