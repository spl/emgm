
module Main where

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
import qualified Bimap          (tests)
import qualified Derive         (tests)

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
           , Bimap.tests
           , Derive.tests
           ]

main =
  do putStrLn "Running tests for EMGM..."
     runTestTT tests

