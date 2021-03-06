{- Print basic elements from PML, as simple strings without formatting
that can be used by output-specific print modules.-} 

module Formal.PML.PrintBasic (findActionsPRIMs, findAgentsPRIMs, findFirstAgentsPRIMs, findAgentsSPECs, findRequiresPRIMs, findRequiresSPECs, findProvidesPRIMs, findProvidesSPECs, printAgentsPRIMs, printFirstAgentsPRIMs, printAgentsSPECs, printRequiresPRIMs, printRequiresSPECs, printProvidesPRIMs, printProvidesSPECs, printID, printSTRING, printTitle, printOPTNM, printScript) where 
import Control.Monad.Reader (Reader(..), ask)
import Data.List (intercalate, intersperse, nub, sort)
import Formal.PML.AbsPML


findActionsPRIMs :: [PRIM] -> [(String, String, String, [String])]
findActionsPRIMs [] = []
findActionsPRIMs (p:ps) = (findActionsPRIM  p) ++ (findActionsPRIMs  ps)

findActionsPRIM :: PRIM -> [(String, String, String, [String])]
findActionsPRIM (PrimSeq  _ ps) = findActionsPRIMs ps
findActionsPRIM (PrimSeln _ ps) = findActionsPRIMs ps
findActionsPRIM (PrimBr   _ ps) = findActionsPRIMs ps
findActionsPRIM (PrimIter _ ps) = findActionsPRIMs ps
findActionsPRIM (PrimTask _ ps) = findActionsPRIMs ps
findActionsPRIM (PrimAct  id t spcs) = [(printID id, printOPTYP t, printScript spcs, concat $ map flattenExpr $ findAgentsSPECs spcs)]


printFirstAgentsPRIMs :: [PRIM] -> [String]
printFirstAgentsPRIMs [] = []
printFirstAgentsPRIMs ps = concat $ map flattenExpr $ findFirstAgentsPRIMs ps

printAgentsPRIMs :: [PRIM] -> [String]
printAgentsPRIMs [] = []
printAgentsPRIMs ps = concat $ map flattenExpr $ findAgentsPRIMs ps

printAgentsSPECs :: [SPEC] -> [String]
printAgentsSPECs [] = []
printAgentsSPECs ps = concat $ map flattenExpr $ findAgentsSPECs ps

printRequiresPRIMs :: [PRIM] -> [String]
printRequiresPRIMs [] = []
printRequiresPRIMs ps = concat $ map flattenExpr $ findRequiresPRIMs ps

printRequiresSPECs :: [SPEC] -> [String]
printRequiresSPECs [] = []
printRequiresSPECs ps = concat $ map flattenExpr $ findRequiresSPECs ps

printProvidesPRIMs :: [PRIM] -> [String]
printProvidesPRIMs [] = []
printProvidesPRIMs ps = concat $ map flattenExpr $ findProvidesPRIMs ps

printProvidesSPECs :: [SPEC] -> [String]
printProvidesSPECs [] = []
printProvidesSPECs ps = concat $ map flattenExpr $ findProvidesSPECs ps

-- Get agent name(s) from expression in 'agent' field: 
-- agent { Doctor && Nurse } -> [Doctor, Nurse]
-- agent { Dev.experience > 5 || QA } -> [Dev, QA]
findAgentsPRIMs :: [PRIM] -> [EXPR]
findAgentsPRIMs = findAgentsPRIMs' False

findFirstAgentsPRIMs :: [PRIM] -> [EXPR]
findFirstAgentsPRIMs = findAgentsPRIMs' True 

findAgentsPRIMs' :: Bool -> [PRIM] -> [EXPR]
findAgentsPRIMs' _ [] = []
findAgentsPRIMs' firstOnly (p:ps) = (findAgentsPRIM firstOnly  p) ++ (findAgentsPRIMs' firstOnly ps)

findAgentsPRIM :: Bool -> PRIM -> [EXPR]
findAgentsPRIM firstOnly (PrimSeq  _ ps) = findAgentsPRIMs' firstOnly  ps
findAgentsPRIM firstOnly (PrimSeln _ ps) = findAgentsPRIMs' firstOnly  ps
findAgentsPRIM firstOnly (PrimBr   _ ps) = findAgentsPRIMs' firstOnly  ps
findAgentsPRIM firstOnly (PrimIter _ ps) = findAgentsPRIMs' firstOnly  ps
findAgentsPRIM firstOnly (PrimTask _ ps) = findAgentsPRIMs' firstOnly  ps
findAgentsPRIM firstOnly (PrimAct  _ _ spcs) = findAgentsSPECs' firstOnly spcs 

findAgentsSPECs :: [SPEC] -> [EXPR]
findAgentsSPECs = findAgentsSPECs' False

findAgentsSPECs' :: Bool -> [SPEC] -> [EXPR]
findAgentsSPECs' _ [] = []
findAgentsSPECs' firstOnly  (s@(SpecAgent e):ss) = if firstOnly then [findFirstExpr e]  else e:(findAgentsSPECs' firstOnly ss)
findAgentsSPECs' firstOnly (_:ss) = findAgentsSPECs' firstOnly ss

--findAgent :: SPEC -> EXPR
--findAgent (SpecAgent e) = e

findFirstExpr :: EXPR -> EXPR
findFirstExpr (DisjExpr l r) = findFirstExpr l
findFirstExpr (ConjExpr l r) = findFirstExpr l
findFirstExpr e              = e

findRequiresPRIMs :: [PRIM] -> [EXPR]
findRequiresPRIMs [] = []
findRequiresPRIMs (p:ps) = (findRequiresPRIM  p) ++ (findRequiresPRIMs  ps)

findRequiresPRIM :: PRIM -> [EXPR]
findRequiresPRIM (PrimSeq  _ ps) = findRequiresPRIMs ps
findRequiresPRIM (PrimSeln _ ps) = findRequiresPRIMs ps
findRequiresPRIM (PrimBr   _ ps) = findRequiresPRIMs ps
findRequiresPRIM (PrimIter _ ps) = findRequiresPRIMs ps
findRequiresPRIM (PrimTask _ ps) = findRequiresPRIMs ps
findRequiresPRIM (PrimAct  _ _ spcs) = findRequiresSPECs spcs

findRequiresSPECs :: [SPEC] -> [EXPR]
findRequiresSPECs  [] = []
findRequiresSPECs  ((SpecReqs e):ss) = e:(findRequiresSPECs ss)
findRequiresSPECs  (_:ss) = findRequiresSPECs ss


findProvidesPRIMs :: [PRIM] -> [EXPR]
findProvidesPRIMs [] = []
findProvidesPRIMs (p:ps) = (findProvidesPRIM  p) ++ (findProvidesPRIMs  ps)

findProvidesPRIM :: PRIM -> [EXPR]
findProvidesPRIM (PrimSeq  _ ps) = findProvidesPRIMs ps
findProvidesPRIM (PrimSeln _ ps) = findProvidesPRIMs ps
findProvidesPRIM (PrimBr   _ ps) = findProvidesPRIMs ps
findProvidesPRIM (PrimIter _ ps) = findProvidesPRIMs ps
findProvidesPRIM (PrimTask _ ps) = findProvidesPRIMs ps
findProvidesPRIM (PrimAct  _ _ spcs) = findProvidesSPECs spcs

findProvidesSPECs :: [SPEC] -> [EXPR]
findProvidesSPECs  [] = []
findProvidesSPECs  (s@(SpecProv e):ss) = e:(findProvidesSPECs ss)
findProvidesSPECs  (_:ss) = findProvidesSPECs ss

findResource :: [SPEC] -> [EXPR]
findResource [] = []
findResource (s@(SpecReqs e):ss) = e:(findResource ss)
findResource (s@(SpecProv e):ss) = e:(findResource ss)
findResource (_:ss) = findResource ss 

printOPTNM :: OPTNM -> String
printOPTNM OpNmNull = "(none)"
printOPTNM (OpNmId id) = printID id

-- XXX Does this really belong here?
printTitle :: PRIM -> String
printTitle p@(PrimSeq  (OpNmId id) ps) = printID id
printTitle p@(PrimSeln (OpNmId id) ps) = printID id
printTitle p@(PrimBr   (OpNmId id) ps) = printID id
printTitle p@(PrimIter (OpNmId id) ps) = printID id
printTitle p@(PrimTask (OpNmId id) ps) = printID id
printTitle p@(PrimAct  id _ _)         = printID id

printID :: ID ->  String
printID (ID n) = n

printOPTYP :: OPTYP -> String
printOPTYP OptNull = "manual"
printOPTYP OptMan = "manual"
printOPTYP OptExec = "automatic"

printSTRING :: STRING ->  String
printSTRING (STRING s) = s

printNUMBER :: NUMBER ->  String
printNUMBER (NUMBER n) = show n

printAgent :: [SPEC] -> [String]
printAgent [] = []
printAgent (s@(SpecAgent e):ss) = printAgent' s ++ printAgent ss 
printAgent (_:ss) = printAgent ss 

printAgent' :: SPEC -> [String]
printAgent' (SpecAgent e) = flattenExpr e
printAgent' _ = ["no agents"]

printResource :: [SPEC] -> [String]
printResource [] = []
printResource (s@(SpecReqs e):ss) = printResource' s ++ printResource ss 
printResource (s@(SpecProv e):ss) = printResource' s ++ printResource ss 
printResource (_:ss) = printResource ss 

printResource' :: SPEC -> [String]
printResource' (SpecReqs e) = flattenExpr e
printResource' (SpecProv e) = flattenExpr e
printResource' _ = ["no resources"]

printScript :: [SPEC] -> String
printScript [] = []
printScript ((SpecScript (STRING s)):ss) = init $ tail s
printScript (_:ss) = printScript ss 


flattenExpr :: EXPR -> [String]

flattenExpr (DisjExpr l r) = (flattenExpr l) ++  (flattenExpr r)
flattenExpr (ConjExpr l r) = (flattenExpr l) ++ (flattenExpr r)
flattenExpr (Str s)        = [printSTRING s] -- XXX a literal string is not really a resource.
flattenExpr (RelEq l r)    = [printVal l, printVal r]
flattenExpr (RelNe l r)    = [printVal l, printVal r]
flattenExpr (RelLt l r)    = [printVal l, printVal r]
flattenExpr (RelGt l r)    = [printVal l, printVal r]
flattenExpr (RelLe l r)    = [printVal l, printVal r]
flattenExpr (RelGe l r)    = [printVal l, printVal r]
flattenExpr (RelVeq l r)   = [printVar l, printVar r]
flattenExpr (RelVne l r)   = [printVar l, printVar r]

flattenExpr (PrimVar v) = [printVar v]
flattenExpr (PrimAttr (Attr v i)) = [] -- attributes are not resources
flattenExpr (PrimNot e) = []           -- not means it's not a resource

printVar :: VAREXPR -> String
printVar (VarId i) = printID i
printVar (VarPar i) = "printVar.VarPar: " ++ printID i
printVar (VarMore qual expr) = printVar expr  -- (qualifier) resource -> resource


printVal :: VALEXPR -> String
printVal (ValAttr a) = printAttr a
printVal (ValString s) = printSTRING s
printVal (ValNum n) = show n

printAttr :: ATTREXPR -> String
printAttr (Attr vexp i) = printVar vexp 
