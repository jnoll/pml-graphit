{- Print basic elements from PML, as simple strings without formatting
that can be used by output-specific print modules.-} 

module Formal.PML.PrintBasic (findAgentsPRIMs, findAgentsSPECs, findRequiresPRIMs, findProvidesPRIMs, printID, printOPTNM) where 
import Control.Monad.Reader (Reader(..), ask)
import Data.List (intercalate, intersperse, nub, sort)
import Formal.PML.AbsPML

-- Get agent name(s) from expression in 'agent' field: 
-- agent { Doctor && Nurse } -> [Doctor, Nurse]
-- agent { Dev.experience > 5 || QA } -> [Dev, QA]
findAgentsPRIMs :: [PRIM] -> [String]
findAgentsPRIMs [] = []
findAgentsPRIMs (p:ps) = (findAgentsPRIM  p) ++ (findAgentsPRIMs  ps)

findAgentsPRIM :: PRIM -> [String]
findAgentsPRIM (PrimSeq  _ ps) = findAgentsPRIMs ps
findAgentsPRIM (PrimSeln _ ps) = findAgentsPRIMs ps
findAgentsPRIM (PrimBr   _ ps) = findAgentsPRIMs ps
findAgentsPRIM (PrimIter _ ps) = findAgentsPRIMs ps
findAgentsPRIM (PrimTask _ ps) = findAgentsPRIMs ps
findAgentsPRIM (PrimAct  _ _ spcs) = findAgentsSPECs spcs


findAgentsSPECs :: [SPEC] -> [String]
--findAgentsSPECs  = printAgent
findAgentsSPECs  [] = []
findAgentsSPECs  (s@(SpecAgent _):ss) = (printAgent' s) ++ printAgent ss
findAgentsSPECs  (_:ss) = printAgent ss

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

printID :: ID ->  String
printID (ID n) = n

printSTRING :: STRING ->  String
printSTRING (STRING s) = s

printNUMBER :: NUMBER ->  String
printNUMBER (NUMBER n) = show n

printAgent :: [SPEC] -> [String]
printAgent [] = []
printAgent (s@(SpecAgent e):ss) = printAgent' s ++ printAgent ss 
printAgent (_:ss) = printAgent ss 

printAgent' :: SPEC -> [String]
printAgent' (SpecAgent e) = printName e
printAgent' _ = ["none"]

printResource :: [SPEC] -> [String]
printResource [] = []
printResource (s@(SpecReqs e):ss) = printResource' s ++ printResource ss 
printResource (s@(SpecProv e):ss) = printResource' s ++ printResource ss 
printResource (_:ss) = printResource ss 

printResource' :: SPEC -> [String]
printResource' (SpecReqs e) = printName e
printResource' (SpecProv e) = printName e
printResource' _ = ["none"]

-- XXXjn this assumes only want first name; suitable for swimlanes
-- based on agent name, but not much else.
printName :: EXPR -> [String]
printName (DisjExpr l r) = (printName l) ++ (printName r) -- only print first agent
printName (ConjExpr l r) = (printName l) ++ (printName r) -- only print first agent
printName (Str s)        = [printSTRING s]
printName (RelEq l _)    = [printVal l]
printName (RelNe l _)    = [printVal l]
printName (RelLt l _)    = [printVal l]
printName (RelGt l _)    = [printVal l]
printName (RelLe l _)    = [printVal l]
printName (RelGe l _)    = [printVal l]
printName (RelVeq l _)   = [printVar l]
printName (RelVne l _)   = [printVar l]

printName (PrimVar v) = [printVar v]
printName (PrimAttr _) = [""]
printName (PrimNot _) = [""]

printVar :: VAREXPR -> String
printVar (VarId i) = printID i
printVar (VarPar _) = ""
printVar (VarMore _ _) = ""

printVal :: VALEXPR -> String
printVal (ValAttr _) = ""
printVal (ValString s) = printSTRING s
printVal (ValNum n) = show n


