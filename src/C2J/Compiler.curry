------------------------------------------------------------------------------
--- This module contains a simple compiler from ICurry to Julia programs.
---
--- @author Michael Hanus
--- @version July 2020
------------------------------------------------------------------------------

module C2J.Compiler
 where

import Char            ( isAlphaNum )
import Directory       ( doesFileExist )
import FilePath        ( (</>) )
import Maybe           ( isJust, isNothing )
import Text.Pretty     ( pPrint )

import ICurry.Types

import C2J.Options
import C2J.PackageConfig ( packagePath )
import ICurry.Read       ( readICurryProg )
import Julia.Pretty      ( ppModule )
import Julia.Types

------------------------------------------------------------------------------

-- Compile a Curry module.
compile :: Options -> String -> IO ()
compile opts0 mname = do
  icprog <- readICurryProg mname (optVerb opts0 == 0)
  let opts     = opts0 { optDemands = collectDemandInfo icprog
                       , optModName = mname }
      mainname = if not (null (optMain opts))
                   then optMain opts
                   else if optExec opts then "main" else ""
      mainfunc = (mname, mainname, 0)
      mbmain   = if not (null mainname) && hasInfo opts mainfunc
                   then Just mainfunc
                   else Nothing
  printStatus opts $ "Compiling Curry program to Julia..."
  when (optExec opts && isNothing mbmain) $
    error $ "Cannot execute, function '" ++ mainname ++ "' not found!"
  when (isJust mbmain && maybe True (/=0) (arityOfFun mainname icprog)) $
    error $ "Main function '" ++ mainname ++ "' has non-zero arity!"
  let priminclude = mname ++ ".julia"
  expriminclude <- doesFileExist priminclude
  let mbinclude = if expriminclude then Just priminclude else Nothing
      jprog     = pPrint (ppModule (c2j opts mbinclude icprog mbmain)) ++ "\n"
      jprogname = mname ++ ".jl"
  printIntermediate opts $ "COMPILED PROGRAM:\n" ++ jprog
  writeFile jprogname jprog
  printStatus opts $ "Compiled Curry program written to: " ++ jprogname

c2j :: Options -> Maybe String -> IProg -> Maybe IQName -> JLModule
c2j opts mbinclude (IProg mn impmods _ fdecls) mbmain = JLModule
  (modName2JL mn)
  [] -- exports
  (rtsmod (optRTS opts) :
   map modName2JL (filter (\m -> m /= "Prelude" || optPrelude opts) impmods))
  ((if optTime opts > 0     then [includeRTS "bench.jl"] else []) ++
   (maybe [] (\f -> [includeFile f]) mbinclude) ++
   concatMap (compileFDecl opts) fdecls ++
   maybe []
         (\mainfun ->
           [JLFDecl runMainFunc [] Nothing
                    [JLPCall "runCurry" (mainArgs mainfun)],
            JLStat withTime] ++
           (if optTime opts > 0
              then [JLStat (JLPCall "benchElapsedTime"
                                    [JLInt (optTime opts), JLSVar runMainFunc])]
              else []))
         mbmain)
 where
  includeFile f = JLStat (JLPCall "include" [JLString f])
  includeRTS  f = includeFile (packagePath </> "include" </> f)

  runMainFunc = "runMain"

  rtsmod PullTabMemo = "CurryRTS"
  rtsmod PullTabOnly = "PulltabOnlyRTS"
  rtsmod PullTab     = "PulltabRTS"
  rtsmod Backtrack   = "BacktrackRTS"

  withTime = if optTime opts >= 0 then JLPCall "@time" [JLFCall runMainFunc []]
                                  else JLPCall runMainFunc []

  mainArgs mainfun =
    [JLString (symbolString mainfun), JLSVar (rewName opts mainfun),
     JLBool (optNormalForm opts), JLBool (optInteractive opts),
     JLBool (optBFS opts), JLBool (optFirst opts),
     JLBool (optVerb opts == 0)]

------------------------------------------------------------------------------
-- Tags used in graph nodes of the Julia run-time code.

tagOp :: Int
tagOp = 0

tagForward :: Int
tagForward = 1

tagChoice :: Int
tagChoice = 2

tagFail :: Int
tagFail = 3

tagFree :: Int
tagFree = 4

tagInt :: Int
tagInt = 5

tagFloat :: Int
tagFloat = 6

tagChar :: Int
tagChar = 7

tagPartOp :: Int
tagPartOp = 8

tagPartConstr :: Int
tagPartConstr = 9

-- Transform a constructor index of ICurry into a tag used in the target program
tagConstr :: IQName -> Int
tagConstr (_,_,ci) = ci + 10

------------------------------------------------------------------------------
-- The string of a function shown in the Julia run-time code.
symbolString :: IQName -> String
symbolString (_,f,_) = f

-- Julia code name for an operation. Depending on the knowledge about
-- demanded arguments, it is either the public code name or the name
-- of the rewrite operation.
codeName :: Options -> IQName -> String
codeName opts ifn =
  (maybe pubName (const rewName) (demandOf opts ifn)) opts ifn

-- Julia code name for public operation (which initiates the evaluation
-- of demanded arguments) for a given function.
pubName :: Options -> IQName -> String
pubName opts (m,f,_) = qname2JL opts "eval_" m f

-- Julia code name for rewrite operation for a given function.
rewName :: Options -> IQName -> String
rewName opts (m,f,_) = qname2JL opts "rew_" m f

-- Encode a qualified name as a Julia identifier with a prefix.
qname2JL :: Options -> String -> String -> String -> String
qname2JL opts pre mname fname =
  if optModName opts == mname
    then pre ++ encodeSpecialChars fname
    else modName2JL mname ++ "." ++ pre ++ encodeSpecialChars fname

-- Encode a module name as a Julia module name.
modName2JL :: String -> String
modName2JL mname = filter (/='.') mname

------------------------------------------------------------------------------

arityOfFun :: String -> IProg -> Maybe Int
arityOfFun fname (IProg _ _  _ fdecls) = lookup fname (map nameArity fdecls)
 where
  nameArity (IFunction (_,f,_) ar _ _ _) = (f,ar)

------------------------------------------------------------------------------

compileFDecl :: Options -> IFunction -> [JLTop]
compileFDecl _    (IFunction _ _ _ _ (IExternal _)) = []
compileFDecl opts (IFunction ifn@(_,f,_) _ vis dmds (IFuncBody b)) =
  case vis of Public  -> [pubOp,rewOp]
              Private -> [rewOp]
 where
  pubOp = JLFDecl (pubName opts ifn) [(0, Just (JLStruct "Node"))] Nothing
            [JLAssign (JLStructAcc (JLIVar 0) "fcode")
                      (JLSVar $ rewName opts ifn),
             case dmds of
               []  -> JLPCall (rewName opts ifn) [JLIVar 0]
               [i] -> JLAssign (JLStructAcc (JLIVar 0) "value") (JLInt (i + 1))
               _   -> functionError opts $ "MULTIPLE DEMANDS!"]
  rewOp = JLFDecl (rewName opts ifn) [(0, Just (JLStruct "Node"))] Nothing
                  (compileBlock opts{ optFunc = f } b)

compileBlock :: Options -> IBlock -> [JLStm]
compileBlock opts (IBlock vardecls assgns stm) =
  map compileVarDecl vardecls ++
  map (compileAssign opts) assgns ++ compileStm opts stm
 where
  compileVarDecl (IVarDecl v) = JLAssign (JLIVar v) $
    if optRTS opts == PullTabOnly then JLSVar "voidNode"
                           else JLFCall "voidNode" [rootTaskId]
  compileVarDecl (IFreeDecl v) = JLAssign (JLIVar v) (genMakeFree opts)

compileAssign :: Options -> IAssign -> JLStm
compileAssign opts (IVarAssign  v e) = JLAssign (JLIVar v) (compileExp opts e)
compileAssign opts (INodeAssign v path e) = case path of
  [p] -> JLAssign (JLArrayAcc (JLStructAcc (JLIVar v) "args") (JLInt (p + 1)))
                  (compileExp opts e)
  _   -> functionError opts "DEEP PATH OCCURRED"

compileExp :: Options -> IExpr -> JLExp
compileExp _ (IVar v) = JLIVar v
compileExp opts (IVarAccess v path) = case path of
  [p] -> JLArrayAcc (JLStructAcc (JLIVar v) "args") (JLInt (p + 1))
  _   -> functionError opts "DEEP PATH OCCURRED"
compileExp opts (ILit (IInt i)) =
  genNode opts tagInt "" i rootTaskId "nothing" []
compileExp opts (ILit (IFloat f)) = genFloatNode opts f rootTaskId
compileExp opts (ILit (IChar c)) =
  genNode opts tagChar "" (ord c) rootTaskId "nothing" []
compileExp opts (IFCall ifn es) =
  let mbdem = demandOf opts ifn
  in genNode opts tagOp (symbolString ifn) (maybe 0 id mbdem) rootTaskId
             (codeName opts ifn) (map (compileExp opts) es)
compileExp opts (ICCall icn es) =
  genNode opts (tagConstr icn) (symbolString icn) 0 rootTaskId "nothing"
          (map (compileExp opts) es)
compileExp opts (IFPCall ifn msng es) =
  -- add a first argument to remember the demand information
  let mbdem = demandOf opts ifn
      inode = genNode opts tagPartOp "" (maybe 0 id mbdem) rootTaskId
                      "nothing" []
  in genNode opts tagPartOp (symbolString ifn) msng rootTaskId
             (codeName opts ifn) (inode : map (compileExp opts) es)
compileExp opts (ICPCall icn msng es) =
  -- add a first argument to remember the constructor index
  let inode = genNode opts (tagConstr icn) "" 0 rootTaskId "nothing" []
  in genNode opts tagPartConstr (symbolString icn) msng rootTaskId "nothing"
             (inode : map (compileExp opts) es)
compileExp opts (IOr e1 e2) =
  genMakeChoice opts (compileExp opts e1) (compileExp opts e2)

compileStm :: Options -> IStatement -> [JLStm]
compileStm _ IExempt =
  [JLAssign (JLStructAcc (JLIVar 0) "tag") (JLInt tagFail)]

compileStm opts (IReturn rexp) = case rexp of
  IVar v ->
    if optRTS opts == Backtrack
      then [JLPCall "setForwardNode" [JLIVar 0, JLIVar v]]
      else [replaceRoot "tag"  (JLInt tagForward),
            JLPCall "empty!" [rootField "args"],
            JLPCall "push!" [rootField "args", JLIVar v]]
  ICCall icn es ->
    setRHS (tagConstr icn) 0 "nothing" (map (compileExp opts) es)
           (symbolString icn)
  IFCall ifn es ->
    setRHS tagOp (maybe 0 id (demandOf opts ifn)) (codeName opts ifn)
           (map (compileExp opts) es) (symbolString ifn)
  IFPCall ifn msng es ->
    -- add a first argument to remember the demand information
    let inode = genNode opts tagPartOp "" (maybe 0 id (demandOf opts ifn))
                        rootTaskId "nothing" []
    in setRHS tagPartOp msng (codeName opts ifn)
              (inode : map (compileExp opts) es) (symbolString ifn)
  ICPCall icn msng es ->
    -- add a first argument to remember the constructor index
    let inode = genNode opts (tagConstr icn) "" 0 rootTaskId "nothing" []
    in setRHS tagPartConstr msng "nothing"
              (inode : map (compileExp opts) es) (symbolString icn)
  IOr e1 e2 -> [JLPCall "setChoice"
                        [JLIVar 0, compileExp opts e1, compileExp opts e2]]
  ILit (IInt i) ->
    if optRTS opts == Backtrack
      then [JLPCall "setConst" [JLIVar 0, JLInt tagInt, JLInt i]]
      else [replaceRoot "tag"   (JLInt tagInt),
            replaceRoot "value" (JLInt i),
            JLPCall "empty!" [rootField "args"]]
  ILit (IFloat f) ->
    if optRTS opts == Backtrack
      then [JLPCall "setFloatConst" [JLIVar 0, JLFloat f]]
      else [replaceRoot "tag"    (JLInt tagFloat),
            replaceRoot "symbol" (JLFloat f),
            JLPCall "empty!" [rootField "args"]]
  ILit (IChar c) ->
    if optRTS opts == Backtrack
      then [JLPCall "setConst" [JLIVar 0, JLInt tagChar, JLInt (ord c)]]
      else [replaceRoot "tag"   (JLInt tagChar),
            replaceRoot "value" (JLInt (ord c)),
            JLPCall "empty!" [rootField "args"]]
  _ -> functionError opts $ "RETURN NOT YET IMPLEMENTED FOR: " ++ show rexp
 where
  rootField field = JLStructAcc (JLIVar 0) field
  replaceRoot field val = JLAssign (rootField field) val

  -- Generate code for replacing redex by a rule's right-hand side:
  setRHS tag value fcode args symbol =
    if optRTS opts == Backtrack
      then [JLPCall "setRHS"
              [JLIVar 0, JLInt tag, JLInt value,
               JLSVar fcode, JLArrayInit args, JLString symbol]]
      else [replaceRoot "tag"   (JLInt tag),
            replaceRoot "value" (JLInt value),
            replaceRoot "fcode" (JLSVar fcode),
            JLPCall "empty!" [rootField "args"]] ++
           map (\a -> JLPCall "push!" [rootField "args", a]) args ++
           [replaceRoot "symbol" (JLString symbol)]

compileStm opts (ICaseCons v branches) = compileBranches branches
 where
  compileBranches [] = -- no test due to complete branches: free variable case
    case branches of
      [] -> compileStm opts IExempt -- no constructor to instantiate
      _  -> [JLPCall "bindVarNode" [JLIVar v, makeChoicesFor branches]]
  compileBranches (IConsBranch icn _ b : bs) =
    [JLIf (JLOp "==" (JLStructAcc (JLIVar v) "tag") (JLInt (tagConstr icn)))
          (compileBlock opts b)
          (compileBranches bs)]

  makeChoicesFor brs = case brs of
    []       -> error "makeChoicesFor with empty list"
    [cb]     -> shallowBindForBranch cb
    (cb:cbs) -> genMakeChoice opts
                  (shallowBindForBranch cb) (makeChoicesFor cbs)

  shallowBindForBranch (IConsBranch icn ar _) =
    genNode opts (tagConstr icn) (symbolString icn) 0 rootTaskId "nothing"
            (map (const (genMakeFree opts)) [1..ar])

compileStm opts (ICaseLit v []) = compileStm opts IExempt -- should not occur
compileStm opts (ICaseLit v branches@(ILitBranch ilit _ : _)) =
  [JLIf (JLOp "==" (JLStructAcc (JLIVar v) "tag") (JLInt (tagOfILit ilit)))
        (compileBranches branches)
        varBinding -- otherwise it must be a free variable
  ]
 where
  tagOfILit (IInt   _) = tagInt
  tagOfILit (IFloat _) = tagFloat
  tagOfILit (IChar  _) = tagChar

  compileBranches [] = compileStm opts IExempt
  compileBranches (ILitBranch ilit b : bs) =
    [JLIf (eqLit ilit) (compileBlock opts b) (compileBranches bs)]
   where
    eqLit (IInt   i) = JLOp "==" (JLStructAcc (JLIVar v) "value")  (JLInt i)
    eqLit (IFloat f) = JLOp "==" (JLStructAcc (JLIVar v) "symbol") (JLFloat f)
    eqLit (IChar  c) = JLOp "==" (JLStructAcc (JLIVar v) "value") 
                                 (JLInt (ord c))

  varBinding = [JLPCall "bindVarNode" [JLIVar v, makeChoicesFor branches]]
   where
    makeChoicesFor brs = case brs of
      []       -> error "makeChoicesFor with empty list"
      [cb]     -> litNodeForBranch cb
      (cb:cbs) -> genMakeChoice opts (litNodeForBranch cb) (makeChoicesFor cbs)
  
    litNodeForBranch (ILitBranch (IInt i) _) =
      genNode opts tagInt "" i rootTaskId "nothing" []
    litNodeForBranch (ILitBranch (IFloat f) _) = genFloatNode opts f rootTaskId
    litNodeForBranch (ILitBranch (IChar c) _) =
      genNode opts tagChar "" (ord c) rootTaskId "nothing" []


-- Generate a `makeFree` to create a new free variable node.
genMakeFree :: Options -> JLExp
genMakeFree opts
  | optRTS opts == PullTabOnly
  = JLFCall "makeFree" [JLInt 0]
  | otherwise
  = JLFCall "makeFree" [rootTaskId]

-- Generate a `makeChoice` to create a new choice node.
genMakeChoice :: Options -> JLExp -> JLExp -> JLExp
genMakeChoice opts a1 a2
  | optRTS opts == PullTabOnly
  = JLFCall "makeChoice" [a1, a2]
  | otherwise
  = JLFCall "makeChoice" [rootTaskId, a1, a2]

genNode :: Options -> Int -> String -> Int -> JLExp -> String ->
           [JLExp] -> JLExp
genNode opts tag symbol value otsk fcode args
  | optRTS opts == PullTabOnly
  = JLFCall "Node" [JLInt tag, JLString symbol, JLInt value,
                    JLSVar fcode, JLArrayInit args]
  | optRTS opts == Backtrack
  = JLFCall "makeNode" [JLInt tag, JLInt value, JLSVar fcode,
                        JLArrayInit args, JLString symbol]
  | otherwise
  = JLFCall "Node" [JLInt tag, JLInt value, otsk, JLSVar "nothing",
                    JLSVar fcode, JLArrayInit args, JLString symbol]

genFloatNode :: Options -> Float -> JLExp -> JLExp
genFloatNode opts value otsk
  | optRTS opts == PullTabOnly
  = JLFCall "Node" [JLInt tagFloat, JLFloat value, JLInt 0,
                    JLSVar "nothing", JLArrayInit []]
  | optRTS opts == Backtrack
  = JLFCall "makeNode" [JLInt tagFloat, JLInt 0, JLSVar "nothing",
                        JLArrayInit [], JLFloat value]
  | otherwise
  = JLFCall "Node" [JLInt tagFloat, JLInt 0, otsk, JLSVar "nothing",
                    JLSVar "nothing", JLArrayInit [], JLFloat value]

rootTaskId :: JLExp
rootTaskId = JLStructAcc (JLIVar 0) "otsk"

------------------------------------------------------------------------------
--- Encode all non-alphanumeric characters in strings by _HEX.
encodeSpecialChars :: String -> String
encodeSpecialChars = concatMap encChar
 where
  encChar c
    | isAlphaNum c = [c]
    | otherwise    = let oc = ord c
                     in ['_', int2hex (oc `div` 16), int2hex(oc `mod` 16)]

  int2hex i = if i<10 then chr (ord '0' + i)
                      else chr (ord 'A' + i - 10)

------------------------------------------------------------------------------
