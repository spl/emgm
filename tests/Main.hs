
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-----------------------------------------------------------------------------

module Main (main) where

import System.Exit (exitSuccess, exitFailure)

import Test.HUnit

import qualified Crush          (tests)
import qualified ReadShow       (tests)
import qualified Compare        (tests)
import qualified Collect        (tests)
import qualified Enum           (tests)
import qualified Everywhere     (tests)
import qualified ZipWith        (tests)
import qualified UnzipWith      (tests)
import qualified Map            (tests)
-- import qualified Bimap          (tests)

-- Make sure the examples compile:
import qualified Ex00StartHere  ()

tests =
  "All" ~: [ Crush.tests
           , ReadShow.tests
           , Compare.tests
           , Collect.tests
           , Enum.tests
           , Everywhere.tests
           , ZipWith.tests
           , UnzipWith.tests
           , Map.tests
--         , Bimap.tests
           ]

main =
  do putStrLn "Running tests for EMGM..."
     counts <- runTestTT tests
     if errors counts > 0 || failures counts > 0
       then exitFailure
       else exitSuccess

