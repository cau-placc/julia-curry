------------------------------------------------------------------------------
--- This module contains a simple compiler from FlatCurry to ICurry programs.
---
--- @author Michael Hanus
--- @version June 2020
------------------------------------------------------------------------------

module Main where

import FilePath          ( (</>), searchPathSeparator )
import System            ( exitWith, getArgs, getEnviron, system )

import System.Path       ( fileInPath )

import C2J.Compiler      ( compile )
import C2J.Options
import C2J.PackageConfig ( packagePath )

------------------------------------------------------------------------------
banner :: String
banner = unlines [bannerLine, bannerText, bannerLine]
 where
   bannerText = "Curry->Julia Compiler (Version of 30/06/20)"
   bannerLine = take (length bannerText) (repeat '=')

main :: IO ()
main = do
  args <- getArgs
  cjoptions <- getEnviron "CJOPTIONS" >>= return . words
  unless (null cjoptions) $
    putStrLn $ "Add standard options: " ++ unwords cjoptions
  (opts,progs) <- processOptions banner (cjoptions ++ args)
  case progs of
    []  -> error "Module name missing"
    [p] -> mainProg opts p
    _   -> error "Too many module names provided"

mainProg :: Options -> String -> IO ()
mainProg opts p = do
  compile opts p
  when (not (null (optMain opts))) $ genExecScript opts
  when (optExec opts) $ do
    exjulia <- fileInPath "julia"
    if exjulia
      then do
        let cmd = setLoadPath opts ++ " && julia " ++ p ++ ".jl"
        printDetails opts $ "Executing command: " ++ cmd
        system cmd
        done
      else do
        putStrLn "Command 'julia' not found in PATH."
        putStrLn "Please install 'julia' to execute target program."
        exitWith 1
 where
  setLoadPath opts = "export JULIA_LOAD_PATH=" ++
                     packagePath </> "include" ++ [searchPathSeparator] ++
                     packagePath </> libdir (optRTS opts)
   where
    libdir PullTabMemo = "lib"
    libdir PullTab     = "lib_pulltab"
    libdir PullTabOnly = "lib_pulltabonly"
    libdir Backtrack   = "lib_backtrack"

  -- Generate a simple shell script to execute the target program
  genExecScript opts = do
    let scriptname = p ++ ".run"
    writeFile scriptname $ unlines
      [ "#!/bin/sh", setLoadPath opts, "julia " ++ p ++ ".jl" ]
    system $ "chmod 755 " ++ scriptname
    printStatus opts $ "Execution script '" ++ scriptname ++ "' generated."

------------------------------------------------------------------------------
