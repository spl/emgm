{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Map
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-----------------------------------------------------------------------------

module Map (tests) where

import Prelude hiding (map)
import qualified Prelude as P (map)
import Data.Char (ord, chr)
import Test.HUnit

import Generics.EMGM as G

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

instance Rep (Map Int) Char where
  rep = Map chr

instance Rep (Map Int) Int where
  rep = Map (+5)

instance Rep (Map Float) Double where
  rep = Map realToFrac

instance Rep (Map String) (Maybe Integer) where
  rep = Map G.read

test_cast1 = "cast (65::Int) :: Char" ~: cast (65::Int) ~?= 'A'
test_cast2 = "cast (-5::Int) :: Int" ~: (cast (-5::Int) :: Int) ~?= 0
test_cast3 = "cast (1::Float) :: Double" ~: (cast (1::Float) :: Double) ~?= 1
test_cast4 = "cast \"37\" :: Maybe Integer" ~: (cast "37" :: Maybe Integer) ~?= Just 37

-----------------------------------------------------------------------------
-- Test collection
-----------------------------------------------------------------------------

tests = "Map" ~:
          [ test_map1
          , test_map2
          , test_map3
          , test_replace1
          , test_cast1
          , test_cast2
          , test_cast3
          , test_cast4
          ]

