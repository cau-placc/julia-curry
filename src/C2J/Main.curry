------------------------------------------------------------------------------
--- This module contains a simple compiler from FlatCurry to ICurry programs.
---
--- @author Michael Hanus
--- @version February 2021
------------------------------------------------------------------------------

module C2J.Main where

import Control.Monad      ( when, unless )
import System.Environment ( getArgs, getEnv )

import System.FilePath    ( (</>), searchPathSeparator )
import System.Path        ( fileInPath )
import System.Process     ( exitWith, system )

import C2J.Compiler       ( compile )
import C2J.Options
import C2J.PackageConfig  ( packagePath )

------------------------------------------------------------------------------
banner :: String
banner = unlines [bannerLine, bannerText, bannerLine]
 where
   bannerText = "Curry->Julia Compiler (Version of 22/02/20)"
   bannerLine = take (length bannerText) (repeat '=')

main :: IO ()
main = do
  args <- getArgs
  cjoptions <- getEnv "CJOPTIONS" >>= return . words
  unless (null cjoptions) $
    putStrLn $ "Add standard options: " ++ unwords cjoptions
  (opts,progs) <- processOptions banner (cjoptions ++ args)
  case progs of
    []  -> printErrorsAndHelpInfo ["Module name missing"]
    [p] -> mainProg opts p
    _   -> printErrorsAndHelpInfo ["Too many module names provided"]

mainProg :: Options -> String -> IO ()
mainProg opts p = do
  compile opts p
  when (optStandalone opts) $ genExecScript opts
  when (optExec opts) $ do
    exjulia <- fileInPath "julia"
    if exjulia
      then do
        let cmd = setLoadPath opts ++ " && julia " ++ p ++ ".jl"
        printVerbose opts $ "Executing command: " ++ cmd
        system cmd
        return ()
      else do
        putStrLn "Command 'julia' not found in PATH."
        putStrLn "Please install 'julia' to execute target program."
        exitWith 1
 where
  setLoadPath opts = "export JULIA_LOAD_PATH=" ++
                     "." ++ [searchPathSeparator] ++
                     packagePath </> "include" ++ [searchPathSeparator] ++
                     packagePath </> libdir (optRTS opts)
   where
    libdir PullTabMemo = "lib"
    libdir PullTab     = "lib_pulltab"
    libdir PullTabOnly = "lib_pulltabonly"
    libdir Backtrack   = "lib_backtrack"

  -- Generate a simple shell script to execute the target program
  genExecScript opts = do
    let scriptname = p
    writeFile scriptname $ unlines
      [ "#!/bin/sh", setLoadPath opts, "julia " ++ p ++ ".jl" ]
    system $ "chmod 755 " ++ scriptname
    printStatus opts $ "Execution script '" ++ scriptname ++ "' generated."

------------------------------------------------------------------------------
