------------------------------------------------------------------------------
--- The options of the Curry to Julia compiler.
---
--- @author Michael Hanus
--- @version July 2020
------------------------------------------------------------------------------

module C2J.Options
 where

import FilePath          ( takeFileName )
import GetOpt
import ReadNumeric       ( readNat )
import System            ( exitWith )

import ICurry.Types
import System.CurryPath  ( stripCurrySuffix )

import C2J.PackageConfig ( packageExecutable )

------------------------------------------------------------------------------
--- The various run-time systems of the compiler.
data RTS = PullTabMemo | PullTab | PullTabOnly | Backtrack
  deriving Eq

------------------------------------------------------------------------------
--- Options for the compiler.
data Options = Options
  { optVerb        :: Int    -- verbosity
                             -- (0: quiet, 1: status, 2: intermediate, 3: all)
  , optHelp        :: Bool   -- if help info should be printed
  , optBFS         :: Bool   -- use breadth-first search strategy
  , optRTS         :: RTS    -- the selected run-time system
  , optExec        :: Bool   -- execute the compiled program?
  , optFirst       :: Bool   -- stop after first value
  , optInteractive :: Bool   -- interactive execution (ask for next result)?
  , optTime        :: Int    -- show execution time (of n runs)?
  , optNormalForm  :: Bool   -- compute normal form of main operation?
  , optPrelude     :: Bool   -- include (big!) Prelude from lib dir?
  , optMain        :: String -- name of main function
  -- internal options
  , optModName     :: String -- name of the curret module
  , optDemands     :: [((String,String),[Int])] -- demand info of functions
  , optFunc        :: String -- name of currently compiled function
  }

defaultOptions :: Options
defaultOptions =
  Options 1 False False PullTabMemo False False False (-1) True True "" "" [] ""

-- Extract demand information from an ICurry program.
collectDemandInfo :: IProg -> [((String,String),[Int])]
collectDemandInfo (IProg _ _ _ fdecls) =
  concatMap funDemand fdecls ++ stdDemands
 where
  funDemand (IFunction (mn,fn,_) _ _ dmds body) = case body of
    IExternal _ -> []
    IFuncBody _ -> [((mn,fn),dmds)]

  stdDemands = [(("Prelude","apply"),[0])]

-- Get demand information for a given ICurry function or `Nothing`
-- if the demand information is unknown (e.g., imported operation).
demandOf :: Options -> IQName -> Maybe Int
demandOf opts (mn,fn,_) =
  maybe Nothing
        (\ds -> case ds of
                  []  -> Just 0
                  [i] -> Just (i + 1)
                  _   -> error $ "MULTIPLE DEMANDS IN FUNCTION " ++ fn)
        (lookup (mn,fn) (optDemands opts))

-- Raise error message for current function.
functionError :: Options -> String -> _
functionError opts msg =
  error $ "Function '" ++ optFunc opts ++ "': " ++ msg

-- Check whether there is demand information for a given ICurry function.
hasInfo :: Options -> IQName -> Bool
hasInfo opts (mn,fn,_) =
  maybe False (const True) (lookup (mn,fn) (optDemands opts))

printStatus :: Options -> String -> IO ()
printStatus opts s = when (optVerb opts > 0) $ putStrLn s

printIntermediate :: Options -> String -> IO ()
printIntermediate opts s = when (optVerb opts > 1) $ putStrLn s

printDetails :: Options -> String -> IO ()
printDetails opts s = when (optVerb opts > 2) $ putStrLn s

------------------------------------------------------------------------------
--- Process the actual command line argument and return the options
--- and the name of the main program.
processOptions :: String -> [String] -> IO (Options,[String])
processOptions banner argv = do
  let (funopts, args, opterrors) = getOpt Permute options argv
      opts = foldl (flip id) defaultOptions funopts
      allopterrors = opterrors ++ checkOptions opts
  unless (null allopterrors)
         (printErrorsAndHelpInfo allopterrors)
  when (optHelp opts) (printUsage >> exitWith 0)
  return (opts, map stripCurrySuffix args)
 where
  printUsage = putStrLn (banner ++ "\n" ++ usageText)

  checkOptions opts
    | optRTS opts == Backtrack && optBFS opts
    = ["do not use `--backtrack' together with `--bfs'\n"]
    | otherwise
    = []

printErrorsAndHelpInfo :: [String] -> IO ()
printErrorsAndHelpInfo es = do
  putStr $ unlines $ map ("ERROR: " ++) es
  putStrLn "Use `--help' for usage infos"
  exitWith 1

-- Help text
usageText :: String
usageText =
  usageInfo ("Usage: " ++ prog ++ " [options] <module name>\n") options
 where
  prog = takeFileName packageExecutable

-- Definition of actual command line options.
options :: [OptDescr (Options -> Options)]
options =
  [ Option "h?" ["help"]
           (NoArg (\opts -> opts { optHelp = True }))
           "print help and exit"
  , Option "q" ["quiet"]
           (NoArg (\opts -> opts { optVerb = 0 }))
           "run quietly (no output, only exit code)"
  , Option "v" ["verbosity"]
            (OptArg (maybe (checkVerb 2) (safeReadNat checkVerb)) "<n>")
            "verbosity level:\n0: quiet (same as `-q')\n1: show status messages (default)\n2: show generated program (same as `-v')\n3: show all details"
  , Option "" ["pulltabmemo"]
           (NoArg (\opts -> opts { optRTS = PullTabMemo }))
           "use RTS for memoized pull-tabbing (default)"
  , Option "" ["pulltab"]
           (NoArg (\opts -> opts { optRTS = PullTab }))
           "use RTS for pure pull-tabbing"
  , Option "" ["pulltabonly"]
           (NoArg (\opts -> opts { optRTS = PullTabOnly }))
           "use reduced translation scheme for pull-tabbing"
  , Option "" ["backtrack"]
           (NoArg (\opts -> opts { optRTS = Backtrack }))
           "use RTS for backtracking"
  , Option "" ["bfs"]
           (NoArg (\opts -> opts { optBFS = True }))
           "use breadth-first search strategy"
  , Option "" ["dfs"]
           (NoArg (\opts -> opts { optBFS = False }))
           "use depth-first search strategy (default)"
  , Option "" ["hnf"]
           (NoArg (\opts -> opts { optNormalForm = False }))
           "compute head normal form (no `normalForm' wrapper)"
  , Option "" ["noprelude"]
           (NoArg (\opts -> opts { optPrelude = False }))
           "do not include Prelude (e.g., for simple tests)"
  , Option "m" ["main"]
           (ReqArg (\s opts -> opts { optMain = s }) "<f>")
           "name of main function to be executed"
  , Option "x" ["execute"]
           (NoArg (\opts -> opts { optExec = True }))
           "execute main function of compiled Julia program"
  , Option "f" ["first"]
           (NoArg (\opts -> opts { optFirst = True }))
           "stop after computing a first value"
  , Option "i" ["interactive"]
           (NoArg (\opts -> opts { optInteractive = True }))
           "interactive execution (ask for next result)"
  , Option "t" ["time"]
            (OptArg (maybe (setTime 0) (safeReadNat setTime)) "<n>")
            "show execution time\n(n>0: average of n runs after one initial run)"
  ]
 where
  safeReadNat opttrans s opts =
   let numError = error "Illegal number argument (try `-h' for help)"
   in maybe numError
            (\ (n,rs) -> if null rs then opttrans n opts else numError)
            (readNat s)

  checkVerb n opts = if n>=0 && n<4
                     then opts { optVerb = n }
                     else error "Illegal verbosity level (try `-h' for help)"

  setTime n opts = opts { optTime = n }

------------------------------------------------------------------------------
