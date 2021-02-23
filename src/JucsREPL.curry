------------------------------------------------------------------------------
--- A REPL for the [Curry->Julia compiler](http://arxiv.org/abs/2008.11999)
--- based on the universal REPL for Curry compilers.
--- 
--- @author  Michael Hanus
--- @version February 2021
------------------------------------------------------------------------------

module JucsREPL where

import Curry.Compiler.Distribution ( installDir )
import Data.List         ( intercalate )
import System.CurryPath ( inCurrySubdir, modNameToPath, sysLibPath )

import REPL.Compiler
import REPL.Main         ( mainREPL )

import C2J.PackageConfig ( packageExecutables, packagePath, packageVersion )

main :: IO ()
main = mainREPL jucs

--- Specification of the Curry->Go compiler:

jucs :: CCDescription
jucs = CCDescription
  "jucs"                               -- the compiler name
  jucsBanner                           -- the banner
  jucsHome                             -- home directory of the compiler
  "info@curry-lang.org"                -- contact email
  (head packageExecutables)            -- compiler executable
  (installDir ++ "/lib")               -- base library path
  False                                -- parser should read untyped FlatCurry
  True                                 -- use CURRYPATH variable
  (\s -> "-v" ++ s)                    -- option to pass verbosity
  (\_ -> "")                           -- option to pass parser options (ignored)
  (\s -> s)                            -- option to compile only
  (\s -> "--standalone -m main " ++ s) -- option to create an executable
  cleanCmd                             -- command to clean module
  [stratOpt, intOpt, firstOpt]
 where
  cleanCmd m =
    "/bin/rm -f " ++ inCurrySubdir m ++ ".* " ++ modNameToPath m ++ ".curry "
                  ++ modNameToPath m ++ ".jl"

jucsHome :: String
jucsHome = packagePath

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

------------------------------------------------------------------------------
