#! /usr/bin/env runhaskell

\begin{code}

{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

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
  , UserHooks(runTests, haddockHook)
  )

import Distribution.Simple.LocalBuildInfo
  ( LocalBuildInfo(withPrograms)
  )

import Distribution.Simple.Program
  ( userSpecifyArgs
  )

main :: IO ()
main = defaultMainWithHooks hooks
  where
    hooks = simpleUserHooks
            { runTests    = runTests'
            , haddockHook = haddockHook'
            }

-- Run a 'test' binary that gets built when configured with '-ftest'.
runTests' _ _ _ _ = system cmd >> return ()
  where testdir = "dist" </> "build" </> "test"
        testcmd = "." </> "test"
        cmd = "cd " ++ testdir ++ " && " ++ testcmd

-- Define __HADDOCK__ for CPP when running haddock. This is a workaround for
-- Haddock not building the documentation due to some issue with Template
-- Haskell.
haddockHook' pkg lbi = haddockHook simpleUserHooks pkg lbi { withPrograms = p }
  where
    p = userSpecifyArgs "haddock" ["--optghc=-D__HADDOCK__"] (withPrograms lbi)

\end{code}

