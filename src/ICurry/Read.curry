------------------------------------------------------------------------------
--- Support for reading ICurry programs.
---
--- @author Michael Hanus
--- @version February 2021
------------------------------------------------------------------------------

module ICurry.Read
 where

import ICurry.Compiler  ( icCompile, defaultICOptions, optVarDecls, optVerb )
import ICurry.Files     ( iCurryFilePath, readICurryFile, writeICurryFile )
import ICurry.Types
import System.CurryPath ( lookupModuleSourceInLoadPath )
import System.Directory ( doesFileExist, getModificationTime )

------------------------------------------------------------------------------
-- Reads an ICurry program if it is newer than the Curry source file,
-- otherwise generate it by calling the ICurry compiler.
-- WARNING: The modification time of the imported modules is not considered.
-- Solution: always call the frontend to generate the FlatCurry file.
readICurryProg :: String -> Bool -> IO IProg
readICurryProg mname quiet =
  lookupModuleSourceInLoadPath mname >>= maybe genIProg checkIProg
 where
  checkIProg (_,cypath) = do
    icpath <- iCurryFilePath mname
    icex <- doesFileExist icpath
    if not icex
      then genIProg
      else do
        cytime <- getModificationTime cypath
        ictime <- getModificationTime icpath
        if ictime > cytime
          then readICurryFile icpath
          else genIProg

  genIProg = do
    let icopts1 = defaultICOptions { optVarDecls = True }
        icopts2 = if quiet then icopts1 { optVerb = 0 } else icopts1
    iprog  <- icCompile icopts2 mname
    icpath <- iCurryFilePath mname
    writeICurryFile icpath iprog
    return iprog

------------------------------------------------------------------------------
