{-# LANGUAGE FlexibleContexts #-}

module Map (tests) where

import Prelude hiding (map)
import qualified Prelude as P (map)
import Data.Char (ord)
import Test.HUnit

import Generics.EMGM

-----------------------------------------------------------------------------
-- Utility functions
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Test functions
-----------------------------------------------------------------------------

test_map1 = "map [Int]" ~: map (*3) [1,2,3::Int] ~?= P.map (*3) [1,2,3]
test_map2 = "map (Maybe Char)" ~: map ord (Just 'a') ~?= Just (ord 'a')
test_map3 = "map (Maybe Char)" ~: map ord Nothing ~?= Nothing

test_replace1 =
  "replace [Float] [[Double]]" ~:
    replace [0.1,0.2,0.3::Float] ([]::[Double]) ~?= [[],[],[]]

-----------------------------------------------------------------------------
-- Test collection
-----------------------------------------------------------------------------

tests = "Map" ~:
          [ test_map1
          , test_map2
          , test_map3
          , test_replace1
          ]

