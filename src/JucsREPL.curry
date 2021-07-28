------------------------------------------------------------------------------
--- A REPL for the [Curry->Julia compiler](http://arxiv.org/abs/2008.11999)
--- based on the universal REPL for Curry compilers.
--- 
--- @author  Michael Hanus
--- @version July 2021
------------------------------------------------------------------------------

module JucsREPL where

import Curry.Compiler.Distribution ( installDir )
import Data.List         ( intercalate, splitOn )

import System.CurryPath  ( inCurrySubdir, modNameToPath, sysLibPath )
import System.FilePath   ( (</>) )

import REPL.Compiler
import REPL.Main         ( mainREPL )

import C2J.PackageConfig ( getPackageExecutables, getPackagePath
                         , packageVersion )

main :: IO ()
main = do
  jucshome <- getJucsHome
  cmpexec  <- getPackageExecutables >>= return . head
  mainREPL (jucs jucshome cmpexec)

--- Specification of the Curry->Go compiler:

jucs :: String -> String -> CCDescription
jucs jucshome cmpexec = CCDescription
  "jucs"                                -- the compiler name
  (compilerMajorVersion, compilerMinorVersion, compilerRevisionVersion)
  jucsBanner                            -- the banner
  jucshome                              -- home directory of the compiler
  "info@curry-lang.org"                 -- contact email
  (installDir ++ "/bin/pakcs-frontend") -- executable of the Curry front end
  cmpexec                               -- compiler executable
  (installDir </> "lib")                -- base library path
  (Just False)                          -- load command reads untyped FlatCurry
  True                                  -- use CURRYPATH variable
  (\s -> "-v" ++ s)                     -- option to pass verbosity
  (\_ -> "")                            -- option to pass parser options
  (\s -> s)                             -- option to compile only
  (\s -> "--standalone -m main " ++ s)  -- option to create an executable
  cleanCmd                              -- command to clean module
  [stratOpt, intOpt, firstOpt]
 where
  cleanCmd m =
    "/bin/rm -f " ++ inCurrySubdir m ++ ".* " ++ modNameToPath m ++ ".curry "
                  ++ modNameToPath m ++ ".jl"

getJucsHome :: IO String
getJucsHome = getPackagePath

jucsBanner :: String
jucsBanner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = "Curry->Julia Interactive Environment (Version " ++
               packageVersion ++ ")"
  bannerLine = take (length bannerText) (repeat '-')

stratOpt :: CCOption
stratOpt = CCOption
  "dfs/bfs     "
  "search strategy (depth-first / breadth-first)"
  [ ("dfs","--dfs")
  , ("bfs","--bfs")
  ]

intOpt :: CCOption
intOpt = CCOption
  "+/-interactive "
  "turn on/off interactive evaluation of main expression"
  [ ("-interactive","")
  , ("+interactive","--interactive")
  ]

firstOpt :: CCOption
firstOpt = CCOption
  "+/-first       "
  "turn on/off printing only first value/solution"
  [ ("-first","")
  , ("+first","--first")
  ]

--- The major version of the compiler.
compilerMajorVersion :: Int
compilerMajorVersion = case reads (head (splitOn "." packageVersion)) of
  [(n,"")] -> n
  _        -> 0

--- The major version of the compiler.
compilerMinorVersion :: Int
compilerMinorVersion = case reads ((splitOn "." packageVersion) !! 1) of
  [(n,"")] -> n
  _        -> 0

--- The major version of the compiler.
compilerRevisionVersion :: Int
compilerRevisionVersion = case reads ((splitOn "." packageVersion) !! 2) of
  [(n,_)] -> n -- maybe there is a suffix with a pre-release identifier
  _       -> 0

------------------------------------------------------------------------------
