{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-- convert PML to PlantUML Activity Diagram
module Formal.PML.PrintUML where
import Control.Monad.Reader (Reader(..), ask)
import Formal.PML.AbsPML
import Data.Char
import Data.List (intercalate, intersperse)

data GraphType = Partitions | Swimlanes | Agents

data GraphOptions = GOpt {
      gopt_graphtype :: GraphType
    , gopt_color :: [(String, String)]
}

defGraphOptions =  GOpt {
                     gopt_graphtype = Partitions
                   , gopt_color = [] -- list if ("action-name", "color") pairs.
}

printUML :: Print a => a -> (Reader GraphOptions) [String]
printUML = prt 

class Print a where
  prt :: a -> (Reader GraphOptions) [String]
--  prtList :: [a] -> String

instance Print PROCESS where
  prt  p = case p of p@(Process id ps) -> printUML' p


graphType :: (Reader GraphOptions) GraphType
graphType = ask >>= (\gopts -> return $ gopt_graphtype gopts)

swimlane :: GraphType -> [PRIM] -> String
swimlane Swimlanes ps = printSwimlane (head $ findAgentsPRIMs ps) 
swimlane _ _ = ""

swimlane' :: GraphType -> [SPEC] -> String
swimlane' Swimlanes ss = printSwimlane (head $ findAgentsSPECs ss)
swimlane' _ _ = ""

printSwimlane :: String -> String
printSwimlane s = "|" ++ s ++ "|"

printPUML :: PRIM -> [PRIM] -> [String] -> (Reader GraphOptions) [String]
printPUML root children body = do
  t <- graphType
  let title = case t of Swimlanes -> printTitle root
                        otherwise -> ""
  return $ intersperse "\n" $ ["@startuml", title, swimlane t children, "start"] ++ body ++ ["end", "@enduml"]

printTitle :: PRIM -> String
printTitle p@(PrimSeq  (OpNmId id) ps) = mkTitle id
printTitle p@(PrimSeln (OpNmId id) ps) = mkTitle id
printTitle p@(PrimBr   (OpNmId id) ps) = mkTitle id
printTitle p@(PrimIter (OpNmId id) ps) = mkTitle id
printTitle p@(PrimTask (OpNmId id) ps) = mkTitle id
printTitle p@(PrimAct  id _ _)         = mkTitle id


mkTitle :: ID -> String
mkTitle id = "title //" ++ (printID id) ++ "//"

printUML' :: PROCESS -> (Reader GraphOptions) [String]
printUML' (Process id ps) = do 
    printPRIMs "\n" ps >>= printPUML (PrimSeq (OpNmId id) ps) ps


printUML'' :: PRIM -> (Reader GraphOptions) [String]
printUML'' p@(PrimSeq  n ps) = printPRIM p >>= printPUML p ps
printUML'' p@(PrimSeln n ps) = printPRIM p >>= printPUML p ps
printUML'' p@(PrimBr   n ps) = printPRIM p >>= printPUML p ps
printUML'' p@(PrimIter n ps) = printPRIM p >>= printPUML p ps
printUML'' p@(PrimTask n ps) = printPRIM p >>= printPUML p ps
printUML'' p@(PrimAct  _ _ _) = printPRIM p >>= printPUML p [p]


printPRIMs :: String -> [PRIM] -> (Reader GraphOptions) [String]
printPRIMs  sep ps = mapM (printPRIM) ps >>= (\ss -> return $ intercalate [sep] ss)


printPRIM :: PRIM -> (Reader GraphOptions) [String]
printPRIM p@(PrimSeq  n ps) = (printPRIMs "" ps) >>= (\ss -> printPRIM' "\n" "\n" n ss p)
printPRIM p@(PrimSeln n ps) = (printSelect ps) >>= (\ss -> printPRIM' "if (select?) then"  "endif" n ss p)
printPRIM p@(PrimBr   n ps) = (printPRIMs "fork again" ps) >>= (\ss -> printPRIM' "fork" "end fork" n ss p)
printPRIM p@(PrimIter n ps) = (printPRIMs "" ps) >>= (\ss -> printPRIM' "repeat"  "repeat while ()" n ss p)
printPRIM p@(PrimTask n ps) = (printPRIMs "split again" ps) >>= (\ss -> printPRIM' "split" "end split" n ss p)
printPRIM p@(PrimAct  n act_t spcs) = ask >>= (\opt -> return $ [(swimlane' (gopt_graphtype opt) spcs) ++ "\n" ++ (printColor (gopt_color opt) (printID n)) ++ ":" ++ (printID n) ++ ";"])

printPRIM' :: String -> String -> OPTNM -> [String] -> PRIM -> (Reader GraphOptions) [String]
printPRIM' pre post id ss p = graphType >>= (\t -> return $ [swimlane t [p], printPartition t id, pre] ++ ss ++ [swimlane t [p], post, printPartitionEnd t id])

printSelect :: [PRIM] -> (Reader GraphOptions) [String]
printSelect [] = return []
printSelect (p:r) | length r == 0 = printPRIM p 
                    | length r == 1 = sequence [printPRIM p, printSelect r] >>= (\ss -> return $ intercalate ["else"] ss)
                    | otherwise = sequence [printPRIM p, printSelect r] >>= (\ss -> return $ intercalate ["elseif (select?) then"] ss)

printPartition :: GraphType ->  OPTNM -> String
printPartition _ OpNmNull =  ""
printPartition Swimlanes (OpNmId n) =  ""
printPartition _ (OpNmId n) =  "partition " ++ (printID n) ++ " {"

printPartitionEnd :: GraphType -> OPTNM -> String
printPartitionEnd _ OpNmNull = ""
printPartitionEnd Swimlanes (OpNmId n) = "" -- Swimlanes 
printPartitionEnd _ (OpNmId _) = "}"

printColor :: [(String, String)] -> String -> String
printColor colors name = case lookup name colors of 
                           Just c -> "#" ++ c
                           Nothing -> ""


printID :: ID ->  String
printID (ID n) = n

printSTRING :: STRING ->  String
printSTRING (STRING s) = s

printNUMBER :: NUMBER ->  String
printNUMBER (NUMBER n) = show n

printAgent :: [SPEC] -> [String]
printAgent [] = []
printAgent (s@(SpecAgent e):ss) = (printAgent' s):printAgent ss 
printAgent (_:ss) = printAgent ss 

printAgent' :: SPEC -> String
printAgent' (SpecAgent e) = printName e
printAgent' _ = "none"

-- XXXjn this assumes only want first name; suitable for swimlanes
-- based on agent name, but not much else.
printName :: EXPR -> String
printName (DisjExpr l _) = (printName l) -- only print first agent
printName (ConjExpr l _) = (printName l) -- only print first agent
printName (Str s) = printSTRING s
printName (RelEq l _) = printVal l
printName (RelNe l _) = printVal l
printName (RelLt l _) = printVal l
printName (RelGt l _) = printVal l
printName (RelLe l _) = printVal l
printName (RelGe l _) = printVal l
printName (RelVeq l _) = printVar l
printName (RelVne l _) = printVar l

printName (PrimVar v) = printVar v
printName (PrimAttr _) = ""
printName (PrimNot _) = ""

printVar :: VAREXPR -> String
printVar (VarId i) = printID i
printVar (VarPar _) = ""
printVar (VarMore _ _) = ""

printVal :: VALEXPR -> String
printVal (ValAttr _) = ""
printVal (ValString s) = printSTRING s
printVal (ValNum n) = show n

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
findAgentsSPECs  (s@(SpecAgent _):ss) = (printAgent' s):printAgent ss
findAgentsSPECs  (_:ss) = printAgent ss

