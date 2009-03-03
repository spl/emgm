#! /usr/bin/env runhaskell

\begin{code}

-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2008, 2009 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-----------------------------------------------------------------------------

module Main (main) where

import System.Cmd (system)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Directory
import System.Environment
import Text.Regex

main :: IO ()
main =
  do as <- getArgs
     jopt <- checkArgs as
     case jopt of
       Nothing  -> printUsage
       Just opt ->
         do exists <- dirsExist
            if not exists
               then
                 putStrLn failureMsg
               else
                 do files <- excludedFiles
                    let cmd = hpc opt files
                    putStrLn cmd
                    exitCode <- system cmd
                    putStrLn $
                      case exitCode of
                        ExitSuccess   -> successMsg opt
                        ExitFailure _ -> ""

-- Directory and file locations

distDir      = "dist"
testsDir     = "tests"
examplesDir  = "examples"
hpcDir       = distDir </> "hpc"
hpcIndex     = hpcDir </> "hpc_index.html"
testExec     = distDir </> "build" </> "test" </> "test"

deriveDir    = "src" </> "Generics" </> "EMGM" </> "Common" </> "Derive"

dirsExist =
  do testDirExists <- doesDirectoryExist testsDir
     examplesDirExists <- doesDirectoryExist examplesDir
     return $ testDirExists && examplesDirExists

excludedFiles :: IO [String]
excludedFiles =
  do testFiles <- getDirectoryContents testsDir
     examplesFiles <- getDirectoryContents examplesDir

     -- The exporting files have nothing important.
     let exportFiles =
           [ "Generics.EMGM.hs"
           , "Generics.EMGM.Common.hs"
           , "Generics.EMGM.Data.hs"
           , "Generics.EMGM.Functions.hs"
           ]

     -- The Base* and Representation files are primarily class declarations.
     let baseFiles =
           [ "Generics.EMGM.Common.Base.hs"
           , "Generics.EMGM.Common.Base2.hs"
           , "Generics.EMGM.Common.Base3.hs"
           , "Generics.EMGM.Common.Representation.hs"
           ]

     -- Template Haskell code is not covered under HPC.
     deriveFiles <- getDirectoryContents deriveDir
     let thFiles =
           "Generics.EMGM.Data.TH.hs" :
           "Generics.EMGM.Common.Derive.hs" :
           map ("Generics.EMGM.Common.Derive." ++) deriveFiles

     return $ testFiles ++ examplesFiles ++ exportFiles ++ baseFiles ++ thFiles

-- Convert file name to module name if Haskell file (.hs)
toModuleName str =
  case matchRegex (mkRegex "(.*)\\.hs$") str of
    Just xs  -> xs
    Nothing  -> []

-- Flags for hpc

toExcludeFlags = unwords . map excludeFlag . concatMap toModuleName
  where excludeFlag = (++) "--exclude="

destFlag = showString "--destdir=" . showString hpcDir

-- Command for hpc
hpc opt files
  = showString "hpc"
  . showChar ' '
  . case opt of
      Markup -> showString "markup " . destFlag
      Report -> showString "report"
  . showChar ' '
  . showString (toExcludeFlags files)
  . showChar ' '
  . showString testExec
  $ ""

data Option = Markup | Report

checkArgs args
  = if length args /= 1
       then return Nothing
       else case head args of
              "markup" -> return $ Just Markup
              "report" -> return $ Just Report
              _        -> return Nothing

printUsage
  = do prog <- getProgName
       putStrLn $ usageMsg prog

-- Messages

usageMsg prog
  = showString "Usage: "
  . showString prog
  . showString " markup|report"
  $ ""

successMsg opt
  = showString "Done." $
      case opt of
        Markup -> showString "\nHPC documentation created: " hpcIndex
        Report -> ""

failureMsg
  = showString "Error: You must be in the top-level EMGM directory to run this command.\n"
  $            "       You should also have both examples/ and tests/ directories."

\end{code}

