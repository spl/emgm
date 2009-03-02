#! /usr/bin/env runhaskell

\begin{code}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module Main (main) where

import System.Cmd
  ( system
  )

import System.FilePath
  ( (</>)
  )

import Data.Version
  ( Version(..)
  )

import Distribution.Simple
  ( defaultMainWithHooks
  , simpleUserHooks
  , UserHooks(runTests, haddockHook, buildHook)
  , Args
  )

import Distribution.Simple.LocalBuildInfo
  ( LocalBuildInfo(..)
  )

import Distribution.Simple.Program
  ( userSpecifyArgs
  )

import Distribution.Simple.Setup
  ( HaddockFlags
  , BuildFlags
  )

import Distribution.Package

import Distribution.PackageDescription
  ( PackageDescription(..)
  , BuildInfo(..)
  , Library(..)
  , Executable(..)
  )

main :: IO ()
main = defaultMainWithHooks hooks
  where
    hooks = simpleUserHooks
            { runTests    = runTests'
            , haddockHook = haddockHook'
            , buildHook   = buildHook'
            }

-- Run a 'test' binary that gets built when configured with '-ftest'.
runTests' :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runTests' _ _ _ _ = system cmd >> return ()
  where testdir = "dist" </> "build" </> "test"
        testcmd = "." </> "test"
        cmd = "cd " ++ testdir ++ " && " ++ testcmd

-- Define __HADDOCK__ for CPP when running haddock. This is a workaround for
-- Haddock not building the documentation due to some issue with Template
-- Haskell.
haddockHook' :: PackageDescription -> LocalBuildInfo -> UserHooks -> HaddockFlags -> IO ()
haddockHook' pkg lbi =
  haddockHook simpleUserHooks pkg (lbi { withPrograms = p })
  where
    p = userSpecifyArgs "haddock" ["--optghc=-D__HADDOCK__"] (withPrograms lbi)

-- Insert CPP flag for building with template-haskell versions >= 2.3. This was
-- previously done in the .cabal file, but it was not backwards compatible with
-- Cabal 1.2. This should work with Cabal from 1.2 to 1.6 at least.
buildHook' :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
buildHook' pkg lbi hooks flags = do
  buildHook simpleUserHooks pkg (lbi { localPkgDescr = newPkgDescr }) hooks flags
  where

    -- Old local package description
    oldPkgDescr = localPkgDescr lbi

    -- New local package description
    newPkgDescr =
      case thVersion of
        Nothing      ->
          oldPkgDescr
        Just version ->
          if version >= Version [2,3] []
          then
            oldPkgDescr
              { library = addThCppToLibrary (library oldPkgDescr)
              , executables = map addThCppToExec (executables oldPkgDescr)
              }
          else
            oldPkgDescr

    -- Template Haskell package name
    thPackageName = mkPackageName "template-haskell"

    mkPackageName :: (Read a) => String -> a
    mkPackageName nm =
      fst $ head $ reads shownNm ++ reads ("PackageName " ++ shownNm)
      where
        shownNm = show nm

    -- template-haskell version
    thVersion = findThVersion (packageDeps lbi)

    -- CPP options for template-haskell >= 2.3
    thCppOpt = "-DTH_LOC_DERIVEREP"

    -- Find the version of the template-haskell package
    findThVersion []          = Nothing
    findThVersion (PackageIdentifier name version:ps)
      | name == thPackageName = Just version
      | otherwise             = findThVersion ps

    -- Add the template-haskell CPP flag to a BuildInfo
    addThCppToBuildInfo :: BuildInfo -> BuildInfo
    addThCppToBuildInfo bi =
      bi { cppOptions = thCppOpt : cppOptions bi }

    -- Add the template-haskell CPP flag to a library package description
    addThCppToLibrary :: Maybe Library -> Maybe Library
    addThCppToLibrary ml = do
      lib <- ml
      return (lib { libBuildInfo = addThCppToBuildInfo (libBuildInfo lib) })

    -- Add the template-haskell CPP flag to an executable package description
    addThCppToExec :: Executable -> Executable
    addThCppToExec exec =
      exec { buildInfo = addThCppToBuildInfo (buildInfo exec) }

\end{code}

