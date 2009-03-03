{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  ZipWith
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-----------------------------------------------------------------------------

module ZipWith (tests) where

import Prelude hiding (zipWith, zip)
import qualified Prelude as P (zipWith, zip)
import Test.HUnit
import Generics.EMGM

-----------------------------------------------------------------------------
-- Tests
-----------------------------------------------------------------------------

test_zipWith1 =
  let ls = [1..7::Int]
      sl = reverse ls
  in "zipWith1" ~: zipWith (+) ls sl ~?= Just (P.zipWith (+) ls sl)

test_zipWith2 =
  let ls = [1..7::Int]
      sl = [1..8::Int]
  in "zipWith2" ~: zipWith (+) ls sl ~?= Nothing

test_zip1 =
  let ls = [1..7::Int]
      sl = reverse ls
  in "zip1" ~: zip ls sl ~?= Just (P.zip ls sl)
  
test_zip2 =
  let ls = [1..7::Int]
      sl = 8 : reverse ls
  in "zip2" ~: zip ls sl ~?= Nothing -- zip does not behave like P.zip.

-----------------------------------------------------------------------------
-- Test collection
-----------------------------------------------------------------------------

tests =
  "ZipWith" ~:
    [ test_zipWith1
    , test_zipWith2
    , test_zip1
    , test_zip2
    ]

