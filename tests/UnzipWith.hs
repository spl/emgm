{-# LANGUAGE FlexibleContexts #-}

module UnzipWith (tests) where

import Prelude hiding (unzip)
import qualified Prelude as P (zip, unzip)
import Test.HUnit
import Generics.EMGM

-----------------------------------------------------------------------------
-- Tests
-----------------------------------------------------------------------------

test_unzipWith1 =
  let ls = [2.5, 1.8::Double]
      split :: Double -> (Int, Double)
      split = properFraction
  in "unzipWith1" ~: unzipWith split ls ~?= ([2,1],[0.5,0.8])

test_unzip1 =
  let ls = [1..7::Int]
      sl = reverse ls
      z = P.zip ls sl
  in "unzip1" ~: unzip z ~?= P.unzip z

-----------------------------------------------------------------------------
-- Test collection
-----------------------------------------------------------------------------

tests =
  "UnzipWith" ~:
    [ test_unzipWith1
    , test_unzip1
    ]

