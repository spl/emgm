#! /usr/bin/env runhaskell

\begin{code}
{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Setup
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-----------------------------------------------------------------------------

module Main (main) where

import System.Cmd
  ( system
  )

import System.FilePath
  ( (</>)
  )

import Distribution.Simple
  ( defaultMainWithHooks
  , simpleUserHooks
  , UserHooks(runTests)
  , Args
  )

import Distribution.Simple.LocalBuildInfo
  ( LocalBuildInfo
  )

import Distribution.PackageDescription
  ( PackageDescription
  )

main :: IO ()
main = defaultMainWithHooks hooks
  where
    hooks = simpleUserHooks
            { runTests    = runTests'
            }

-- Run a 'test' binary that gets built when configured with '-ftest'.
runTests' :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runTests' _ _ _ _ = system cmd >> return ()
  where testdir = "dist" </> "build" </> "test"
        testcmd = "." </> "test"
        cmd = "cd " ++ testdir ++ " && " ++ testcmd

\end{code}

