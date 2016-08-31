{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-- convert PML to PlantUML Activity Diagram
module Formal.PML.PrintUML (GraphType(..), GraphOptions(..), defGraphOptions, printUMLProcess, printUMLPRIM) where
import Formal.PML.PrintBasic 
import Control.Monad.Reader (Reader(..), ask)
import Formal.PML.AbsPML
import Data.Char
import Data.List (nub, intercalate, intersperse, sort)
import Text.Pandoc (readMarkdown, writePlain, ReaderOptions(..), WriterOptions(..), def)


data GraphType = Partitions | Swimlanes | Agents 
  deriving (Eq, Show)

data GraphOptions = GOpt {
      gopt_color :: [(String, String)]
    , gopt_graphtype :: GraphType
    , gopt_prunedepth :: Int
    , gopt_textwidth :: Int
}

defGraphOptions =  GOpt {
                     gopt_graphtype = Partitions
                   , gopt_prunedepth = 0
                   , gopt_color = [] -- list if ("action-name", "color") pairs.
                   , gopt_textwidth = 10 -- "fill" column
}

optGraphType :: (Reader GraphOptions) GraphType
optGraphType = ask >>= (\gopts -> return $ gopt_graphtype gopts)

optPruneDepth :: (Reader GraphOptions) Int
optPruneDepth = ask >>= (\gopts -> return $ gopt_prunedepth gopts)

-- Entry points.
printUMLProcess :: PROCESS -> (Reader GraphOptions) [String]
printUMLProcess (Process id ps) = printPRIMs 0 "\n" ps >>= printPUML (PrimSeq (OpNmId id) ps) ps

printUMLPRIM :: PRIM  -> (Reader GraphOptions) [String]
printUMLPRIM p = case p of 
                   p@(PrimSeq  n ps)  -> printPRIM 0 p >>= printPUML p ps
                   p@(PrimSeln n ps)  -> printPRIM 0 p >>= printPUML p ps
                   p@(PrimBr   n ps)  -> printPRIM 0 p >>= printPUML p ps
                   p@(PrimIter n ps)  -> printPRIM 0 p >>= printPUML p ps
                   p@(PrimTask n ps)  -> printPRIM 0 p >>= printPUML p ps
                   p@(PrimAct  _ _ _) -> printPRIM 0 p >>= printPUML p [p]

printPUML :: PRIM -> [PRIM] -> [String] -> (Reader GraphOptions) [String]
printPUML root children body = do
  t <- optGraphType
  case t of Agents -> return $ sort $ nub $ findAgentsPRIMs children
            otherwise -> return $ intersperse "\n" $ 
                               ["@startuml", printTitle root] 
                               ++ (sort $ swimlanesPRIMs t children) -- print ALL swimlanes at top so we see all agents
                               ++ ["start"] ++ body ++ ["end", "@enduml"]


-- Print all swimlanes
swimlanesPRIMs :: GraphType -> [PRIM] -> [String]
swimlanesPRIMs Swimlanes ps = map printSwimlane $ nub $ findAgentsPRIMs ps
swimlanesPRIMs _ _ = [""]

-- This is necessary to get the control flow construct in the right swimlane.
swimlanePRIMs :: GraphType -> [PRIM] -> String
swimlanePRIMs Swimlanes ps = let as = findAgentsPRIMs ps in 
                             if length as > 0 then printSwimlane (head as) else printSwimlane "(none)"
swimlanePRIMs _ _ = ""

swimlaneSPECs :: GraphType -> [SPEC] -> String
swimlaneSPECs Swimlanes ss = let as = findAgentsSPECs ss in 
                             if length as > 0 then printSwimlane (head as) else printSwimlane "(none)"
swimlaneSPECs _ _ = ""

printSwimlane :: String -> String
printSwimlane s = "|" ++ s ++ "|"



printTitle :: PRIM -> String
printTitle p@(PrimSeq  (OpNmId id) ps) = mkTitle id
printTitle p@(PrimSeln (OpNmId id) ps) = mkTitle id
printTitle p@(PrimBr   (OpNmId id) ps) = mkTitle id
printTitle p@(PrimIter (OpNmId id) ps) = mkTitle id
printTitle p@(PrimTask (OpNmId id) ps) = mkTitle id
printTitle p@(PrimAct  id _ _)         = mkTitle id


mkTitle :: ID -> String
mkTitle id = "title //" ++ (ustosp $ printID id) ++ "//"

ustosp :: String -> String
ustosp = map (\c -> if c == '_' then ' ' else c)

formatString :: Int -> String -> String
formatString w s =  intercalate "\\n" $ lines $ formatString' w s

formatString' :: Int -> String -> String
formatString' w s =  case readMarkdown def $ ustosp s of
                    Left e -> "fail"
                    Right p -> writePlain def { writerColumns = w } p




printPRIMs :: Int ->  String -> [PRIM] -> (Reader GraphOptions) [String]
printPRIMs depth sep ps = mapM (printPRIM depth) ps >>= (\ss -> return $ intercalate [sep] ss)


printPRIM :: Int -> PRIM -> (Reader GraphOptions) [String]
printPRIM depth p@(PrimSeq  n ps) = (printPRIMs (depth + 1) "" ps) >>= (\ss -> printPRIM' depth "\n" "\n" n ss p)
printPRIM depth p@(PrimSeln n ps) = (printSelect (depth + 1) ps) >>= (\ss -> printPRIM' depth "if (select?) then"  "endif" n ss p)
printPRIM depth p@(PrimBr   n ps) = (printPRIMs (depth + 1) "fork again" ps) >>= (\ss -> printPRIM' depth "fork" "end fork" n ss p)
printPRIM depth p@(PrimIter n ps) = (printPRIMs (depth + 1) "" ps) >>= (\ss -> printPRIM' depth "repeat"  "repeat while ()" n ss p)
printPRIM depth p@(PrimTask n ps) = (printPRIMs (depth + 1) "split again" ps) >>= (\ss -> printPRIM' depth "split" "end split" n ss p)
printPRIM depth p@(PrimAct id act_t spcs) = 
    ask >>= (\opt -> if gopt_prunedepth opt > 0 then
                         if depth <= gopt_prunedepth opt then 
                         return $ printAct opt id spcs ";"
                         else return [""]
                     else return $ printAct opt id spcs ";")

printAct :: GraphOptions -> ID -> [SPEC] -> String -> [String]
printAct opt id spcs term = if (gopt_graphtype opt) == Swimlanes then printActSwimlane (gopt_color opt) (gopt_textwidth opt) id spcs term
                            else printActPlain (gopt_color opt) (gopt_textwidth opt) id spcs term

printActPlain :: [(String, String)] -> Int -> ID -> [SPEC] -> String -> [String]
printActPlain c w id spcs term = [(printColor c (printID id)) ++  printActName w id term]
              
printActSwimlane :: [(String, String)] -> Int -> ID -> [SPEC] -> String -> [String]
printActSwimlane c w id spcs term =
    let as = findAgentsSPECs spcs
        a = if length as > 0 then head as else "(none)"
    in if length as > 1         -- add a UML note with other agent names.
       then [printSwimlane a, (printColor c $ printID id) ++ printActName w id term, "note", 
--                               formatString' w  ("also involved: " ++ (intercalate " " $ printActSwimlane' w id $ tail as)), 
                               formatString' w  ("also involved: " ++ (intercalate ", " $ tail as)), 
                               "end note"
            ]
       else [printSwimlane a, (printColor c $ printID id) ++ printActName w id term]


-- Recursively print actions in swimlanes for each agent.
printActSwimlane' :: Int -> ID -> [String] -> [String]
printActSwimlane' _ _ [] = [""]
printActSwimlane' w id (a:as) = [a ++ ", "] ++ printActSwimlane' w id as

printActName :: Int -> ID -> String -> String
printActName w id term = ":" ++ (formatString w $ printID id) ++ term

printPseudoAct :: GraphOptions -> OPTNM -> [String]
printPseudoAct opt n = printAct opt (ID $ printOPTNM n) [] "|"

printPRIM' :: Int -> String -> String -> OPTNM -> [String] -> PRIM -> (Reader GraphOptions) [String]
printPRIM' depth pre post n ss p = 
    optPruneDepth >>= (\d -> if d > 0 then 
                                 if depth < d then 
                                     printPRIM'' pre post n ss p
                                 else if depth == d then ask >>= (\opt -> return $ printPseudoAct opt n)
                                      else return [""]
                             else printPRIM'' pre post n ss p)

printPRIM'' :: String -> String -> OPTNM -> [String] -> PRIM -> (Reader GraphOptions) [String]
printPRIM'' pre post id ss p = 
    ask >>= (\opt -> let t = gopt_graphtype opt in 
                     return $ [swimlanePRIMs t [p], printPartition opt id, pre] ++ ss ++ [swimlanePRIMs t [p], post, printPartitionEnd t id])

printSelect :: Int -> [PRIM] -> (Reader GraphOptions) [String]
printSelect _ [] = return []
printSelect depth (p:r) 
  | length r == 0 = printPRIM depth p 
  | length r == 1 = sequence [printPRIM depth p, printSelect depth r] >>= (\ss -> return $ intercalate ["else"] ss) 
  | otherwise = sequence [printPRIM depth p, printSelect depth r] >>= (\ss -> return $ intercalate ["elseif (select?) then"] ss) 

printPartition :: GraphOptions ->  OPTNM -> String
printPartition _ OpNmNull =  ""
printPartition opt (OpNmId n) = case gopt_graphtype opt of 
                                  Swimlanes -> ""
                                  otherwise -> "partition \"" ++ (formatString (gopt_textwidth opt) $ printID n) ++ "\" {"

printPartitionEnd :: GraphType -> OPTNM -> String
printPartitionEnd _ OpNmNull = ""
printPartitionEnd Swimlanes (OpNmId n) = "" -- Swimlanes 
printPartitionEnd _ (OpNmId _) = "}"

printColor :: [(String, String)] -> String -> String
printColor colors name = case lookup name colors of 
                           Just c -> "#" ++ c
                           Nothing -> ""

