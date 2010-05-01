-----------------------------------------------------------------------------
-- |
-- Module      :  ReadShow
-- Copyright   :  (c) 2008 - 2010 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}

module ReadShow (tests) where

import Prelude hiding (Read, Show, readsPrec, reads, read, show)
import qualified Prelude as P (Read, Show, read, show)
import Data.Generics (Data)
import Test.HUnit

import Generics.EMGM
import Generics.EMGM.Functions.Read
import Generics.EMGM.Functions.Show

import Base
import A

-----------------------------------------------------------------------------
-- Utility functions
-----------------------------------------------------------------------------

test_all :: (Eq a, Data a, P.Read a, P.Show a, Rep Read a, Rep Show a)
         => Bool -> a -> Test
test_all notIfxRec x =
  let expected = P.show x
      actual   = show x in
  "x = " ++ P.show x ++ " :: " ++ typeNameOf x ~:

    -- The following is conditional on whether x has an infix constructor with
    -- record syntax. That is currently broken in GHC.
    (if notIfxRec then ["P.read . show == id" ~: P.read actual ~?= x] else [])
    ++
    [ "read . P.show == id" ~: read expected ~?= Just x
    , "read . show == id"   ~: read actual ~?= Just x
    , "show == P.show"      ~: actual ~?= expected
    ]

-----------------------------------------------------------------------------
-- Test collection
-----------------------------------------------------------------------------

tests =
  "ReadShow" ~:
    [ test_all True  (42 :: Int)
    , test_all True  (9999999999999999999999999999999 :: Integer)
    , test_all True  (4.2 :: Float)
    , test_all True  (5.3 :: Double)
    , test_all True  ('\t' :: Char)
    , test_all True  (True :: Bool)
    , test_all True  (Just True :: Maybe Bool)
    , test_all True  (Left 7.8888 :: Either Float Char)
    , test_all True  (Right '2' :: Either Float Char)
    , test_all True  (Nothing :: Maybe Double)
    , test_all True  (Just 256 :: Maybe Int)
    , test_all True  (A1 5 :: A Int)
    , test_all True  (A1 (Just 5) :: A (Maybe Int))
    , test_all True  (A2 88 (A1 99) :: A Int)
    , test_all True  (A3 654 :: A Int)
    , test_all True  (Just (A3 654) :: Maybe (A Int))
    , test_all True  (A4 (A2 1 (A3 2)) 3 :: A Int)
    , test_all True  (A5 'a' (A4 (A3 102) 103) 104 :: A Int)
    , test_all True  (A3 8.0 :^: 8.0 :: A Char)
    , test_all True  ((A3 (-0.2) :^: 0.2) :^: 2.0 :: A Char)
    , test_all False (A1 1.1 :<>: A1 1.2 :^: 1.3 :: A Float)
    , test_all False (A1 (A3 8.8 :^: 9.9) :<>: A4 (A4 (A2 101 (A1 (A1 22.22))) (-1)) 55 :: A (A Float))
    , test_all True  [1,2,3,4,5 :: Int]
    , test_all True  [[5.3,3.5],[35.0],[0.53 :: Float]]
    , test_all True  "abcdefgh"
    , test_all True  (Just "abcdefgh")
    , test_all True  ()
    , test_all True  (1::Int,2::Float)
    , test_all True  (1::Int,2::Float,3::Double)
    , test_all True  (1::Int,2::Float,3::Double,'4')
    , test_all True  (1::Int,2::Float,3::Double,'4',False)
    , test_all True  (1::Int,2::Float,3::Double,'4',False,Just (6::Int))
    , test_all True  (1::Int,2::Float,3::Double,'4',False,Just (6::Int),A1 (7::Float))
    ]

