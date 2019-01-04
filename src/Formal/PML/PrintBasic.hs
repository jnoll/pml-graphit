{- Print basic elements from PML, as simple strings without formatting
that can be used by output-specific print modules.-} 

module Formal.PML.PrintBasic (findActionsPRIMs, findAgentsPRIMs, findFirstAgentsPRIMs, findAgentsSPECs, findRequiresPRIMs, findRequiresSPECs, findProvidesPRIMs, findProvidesSPECs, printID, printTitle, printOPTNM, printScript) where 
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
findActionsPRIM (PrimAct  id t spcs) = [(printID id, printOPTYP t, printScript spcs, findAgentsSPECs spcs)]



-- Get agent name(s) from expression in 'agent' field: 
-- agent { Doctor && Nurse } -> [Doctor, Nurse]
-- agent { Dev.experience > 5 || QA } -> [Dev, QA]
findAgentsPRIMs :: [PRIM] -> [String]
findAgentsPRIMs = findAgentsPRIMs' False

findFirstAgentsPRIMs :: [PRIM] -> [String]
findFirstAgentsPRIMs = findAgentsPRIMs' True 

findAgentsPRIMs' :: Bool -> [PRIM] -> [String]
findAgentsPRIMs' _ [] = []
findAgentsPRIMs' firstOnly (p:ps) = (findAgentsPRIM firstOnly  p) ++ (findAgentsPRIMs' firstOnly ps)

findAgentsPRIM :: Bool -> PRIM -> [String]
findAgentsPRIM firstOnly (PrimSeq  _ ps) = findAgentsPRIMs' firstOnly  ps
findAgentsPRIM firstOnly (PrimSeln _ ps) = findAgentsPRIMs' firstOnly  ps
findAgentsPRIM firstOnly (PrimBr   _ ps) = findAgentsPRIMs' firstOnly  ps
findAgentsPRIM firstOnly (PrimIter _ ps) = findAgentsPRIMs' firstOnly  ps
findAgentsPRIM firstOnly (PrimTask _ ps) = findAgentsPRIMs' firstOnly  ps
findAgentsPRIM firstOnly (PrimAct  _ _ spcs) = findAgentsSPECs' firstOnly spcs 

findAgentsSPECs :: [SPEC] -> [String]
findAgentsSPECs = findAgentsSPECs' False

findAgentsSPECs' :: Bool -> [SPEC] -> [String]
findAgentsSPECs' _ [] = []
findAgentsSPECs' firstOnly  (s@(SpecAgent _):ss) = if firstOnly then [head $ printAgent' s]  else (printAgent' s) ++ findAgentsSPECs' firstOnly ss
findAgentsSPECs' firstOnly (_:ss) = findAgentsSPECs' firstOnly ss

-- Get resource name(s) from expression in 'resource' field. 
findRequiresPRIMs :: [PRIM] -> [String]
findRequiresPRIMs [] = []
findRequiresPRIMs (p:ps) = (findRequiresPRIM  p) ++ (findRequiresPRIMs  ps)

findRequiresPRIM :: PRIM -> [String]
findRequiresPRIM (PrimSeq  _ ps) = findRequiresPRIMs ps
findRequiresPRIM (PrimSeln _ ps) = findRequiresPRIMs ps
findRequiresPRIM (PrimBr   _ ps) = findRequiresPRIMs ps
findRequiresPRIM (PrimIter _ ps) = findRequiresPRIMs ps
findRequiresPRIM (PrimTask _ ps) = findRequiresPRIMs ps
findRequiresPRIM (PrimAct  _ _ spcs) = findRequiresSPECs spcs


findRequiresSPECs :: [SPEC] -> [String]
--findRequiresSPECs  = printAgent
findRequiresSPECs  [] = []
findRequiresSPECs  (s@(SpecReqs _):ss) = (printResource' s) ++ findRequiresSPECs ss
findRequiresSPECs  (_:ss) = findRequiresSPECs ss

findProvidesPRIMs :: [PRIM] -> [String]
findProvidesPRIMs [] = []
findProvidesPRIMs (p:ps) = (findProvidesPRIM  p) ++ (findProvidesPRIMs  ps)

findProvidesPRIM :: PRIM -> [String]
findProvidesPRIM (PrimSeq  _ ps) = findProvidesPRIMs ps
findProvidesPRIM (PrimSeln _ ps) = findProvidesPRIMs ps
findProvidesPRIM (PrimBr   _ ps) = findProvidesPRIMs ps
findProvidesPRIM (PrimIter _ ps) = findProvidesPRIMs ps
findProvidesPRIM (PrimTask _ ps) = findProvidesPRIMs ps
findProvidesPRIM (PrimAct  _ _ spcs) = findProvidesSPECs spcs


findProvidesSPECs :: [SPEC] -> [String]
findProvidesSPECs  [] = []
findProvidesSPECs  (s@(SpecProv _):ss) = (printResource' s) ++ findProvidesSPECs ss
findProvidesSPECs  (_:ss) = findProvidesSPECs ss


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
printAgent' (SpecAgent e) = printExpr e
printAgent' _ = ["none"]

printResource :: [SPEC] -> [String]
printResource [] = []
printResource (s@(SpecReqs e):ss) = printResource' s ++ printResource ss 
printResource (s@(SpecProv e):ss) = printResource' s ++ printResource ss 
printResource (_:ss) = printResource ss 

printResource' :: SPEC -> [String]
printResource' (SpecReqs e) = printExpr e
printResource' (SpecProv e) = printExpr e
printResource' _ = ["none"]

printScript :: [SPEC] -> String
printScript [] = []
printScript ((SpecScript (STRING s)):ss) = init $ tail s
printScript (_:ss) = printScript ss 


printExpr :: EXPR -> [String]

printExpr (DisjExpr l r) = [(concat $ printExpr l) ++ " or " ++ (concat $ printExpr r)] -- XXX s.b. "lhs or rhs"
printExpr (ConjExpr l r) = (printExpr l) ++ (printExpr r)
printExpr (Str s)        = [printSTRING s ++ " exists"]
printExpr (RelEq l r)    = [printVal l ++ " is " ++ printVal r]
printExpr (RelNe l r)    = [printVal l ++ " is not " ++ printVal r]
printExpr (RelLt l r)    = [printVal l ++ " is less than " ++ printVal r]
printExpr (RelGt l r)    = [printVal l ++ " is greater than " ++ printVal r]
printExpr (RelLe l r)    = [printVal l ++ " is less than or equal to " ++ printVal r]
printExpr (RelGe l r)    = [printVal l ++ " is greater than or equal to " ++ printVal r]
printExpr (RelVeq l r)   = [printVar l ++ " is equal to " ++ printVar r]
printExpr (RelVne l r)   = [printVar l ++ " is not equal to " ++ printVar r]

printExpr (PrimVar v) = [printVar v]
printExpr (PrimAttr (Attr v i)) = [printVar v ++ " " ++ printID i] -- assumes attr is a past-tense verb
printExpr (PrimNot e) = printExpr e

printVar :: VAREXPR -> String
printVar (VarId i) = printID i
printVar (VarPar i) = "printVar.VarPar: " ++ printID i
printVar (VarMore qual expr) = printVar expr ++ "|(" ++ printID qual ++ ")" -- (qualifier) resource

printAttr :: ATTREXPR -> String
printAttr (Attr vexp i) = printID i ++ " of " ++ printVar vexp 

printVal :: VALEXPR -> String
printVal (ValAttr a) = printAttr a
printVal (ValString s) = printSTRING s
printVal (ValNum n) = show n
