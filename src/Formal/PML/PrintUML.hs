{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-- convert PML to PlantUML Activity Diagram
module Formal.PML.PrintUML (printUMLProcess, printUMLPRIM) where
import Formal.PML.GraphOptions (GraphType(..), GraphOptions(..), defGraphOptions, optGraphType, optPruneDepth, optSwimlaneHeadings)
import Formal.PML.PrintBasic 
import Control.Monad.Reader (Reader(..), ask)
import Formal.PML.AbsPML
import Data.Char
import Data.List (drop, find, group, intercalate, intersperse, nub, sort, sortBy, take, isPrefixOf)
import Data.Maybe (isJust, fromJust)
import Text.Pandoc (readMarkdown, writePlain, ReaderOptions(..), WriterOptions(..), def)
import Text.Pandoc.Shared (trim) -- overkill, but Pandoc is included anyway

-- Entry points.
printPUML :: PRIM -> [PRIM] -> [String] -> (Reader GraphOptions) String
printPUML root children body = do
  t <- optGraphType
  cols <- optSwimlaneHeadings
  gopts <- ask
  let ags = if null cols then printFirstAgentsPRIMs children else cols -- XXX sb printFirstAgentsPRIMs
      swimlanes = if null cols then swimlanesPRIMs t ags children else printSwimLanes cols 
  return $ intercalate "\n" $ ["@startuml", mkTitle root (gopt_titleprefix gopts)] 
                               ++ (swimlanes) -- print ALL swimlanes at top so we see all agents
                               ++ [printSwimlane $ if null ags then "(none)" else head ags, "start"] ++ body ++ ["end", "@enduml"]


printUMLProcess :: PROCESS -> (Reader GraphOptions) String
printUMLProcess (Process id ps) = printPRIMs 0 "\n" ps >>= printPUML (PrimSeq (OpNmId id) ps) ps
                                  

printUMLPRIM :: PRIM  -> (Reader GraphOptions) String
printUMLPRIM p = case p of 
                   p@(PrimSeq  n ps)  -> printUMLPRIM' p ps
                   p@(PrimSeln n ps)  -> printUMLPRIM' p ps
                   p@(PrimBr   n ps)  -> printUMLPRIM' p ps
                   p@(PrimIter n ps)  -> printUMLPRIM' p ps
                   p@(PrimTask n ps)  -> printUMLPRIM' p ps
                   p@(PrimAct  _ _ _) -> printUMLPRIM' p [p]

printUMLPRIM' :: PRIM -> [PRIM] -> (Reader GraphOptions) String
printUMLPRIM' p ps = printPRIM 0 p >>= printPUML p ps


-- Print all swimlanes
swimlanesPRIMs :: GraphType -> [String] -> [PRIM] -> [String]
swimlanesPRIMs Swimlanes agents ps =
                              if null agents then [printSwimlane "(none)"]
                              else let freqs = sortBy (\(_,f) (_,f') -> compare f f') $ map (\x->(head x, length x)) . group . sort $ agents
                                       rlanes = take (quot (length freqs) 2) freqs
                                       llanes = reverse $ drop (quot (length freqs) 2) freqs
                                   in printSwimLanes $ map (\(s, _) -> s) $ (rlanes ++ llanes)
swimlanesPRIMs _ _ _ = [""]

-- This is necessary to get the control flow construct in the right swimlane.
swimlanePRIMs :: GraphType -> [PRIM] -> String
swimlanePRIMs Swimlanes ps = let as = printAgentsPRIMs ps in 
                             if length as > 0 then printSwimlane (head as) else printSwimlane "(none)"
swimlanePRIMs _ _ = ""

swimlaneSPECs :: GraphType -> [SPEC] -> String
swimlaneSPECs Swimlanes ss = let as = printAgentsSPECs ss in 
                             if length as > 0 then printSwimlane (head as) else printSwimlane "(none)"
swimlaneSPECs _ _ = ""

printSwimLanes :: [String] -> [String]
printSwimLanes ss = map printSwimlane ss

printSwimlane :: String -> String
printSwimlane s = "|" ++ s ++ "|"





mkTitle :: PRIM -> Maybe String -> String
mkTitle p prefix = "title //" ++ (if isJust prefix then (ustosp $ fromJust prefix) ++ ": " else "") ++ (ustosp $ printTitle p) ++ "//"

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

shouldExpand :: GraphOptions -> OPTNM -> Bool
shouldExpand opts (OpNmId (ID id)) = (not $ null $ (gopt_expand opts)) && isPrefixOf (gopt_expand opts) id 
shouldExpand _ _ = False

expandDepth :: GraphOptions -> OPTNM -> Int -> Int
expandDepth opts n cur = if shouldExpand opts n then 0 else cur + 1

printPRIM :: Int -> PRIM -> (Reader GraphOptions) [String]
printPRIM depth p@(PrimSeq  n ps) = ask >>= (\opts -> (return $ expandDepth opts n depth)) >>= (\depth' -> printPRIMs depth'  "" ps >>= (\ss -> printPRIM' depth' "\n" "\n" n ss p))
printPRIM depth p@(PrimSeln n ps) = ask >>= (\opts -> (return $ expandDepth opts n depth)) >>= (\depth' -> printSelect depth' ps >>= (\ss -> printPRIM' depth' "if (do) then (either)"  "endif" n ss p))
printPRIM depth p@(PrimBr   n ps) = ask >>= (\opts -> (return $ expandDepth opts n depth)) >>= (\depth' -> printPRIMs  depth' "fork again" ps >>= (\ss -> printPRIM' depth' "fork" "end fork" n ss p))
printPRIM depth p@(PrimIter n ps) = ask >>= (\opts -> (return $ expandDepth opts n depth)) >>= (\depth' -> printPRIMs  depth' "" ps >>= (\ss -> printPRIM' depth' "repeat"  "repeat while (again?) is (yes)" n ss p))
printPRIM depth p@(PrimTask n ps) = ask >>= (\opts -> (return $ expandDepth opts n depth)) >>= (\depth' -> printPRIMs  depth' "split again" ps >>= (\ss -> printPRIM' depth' "split" "end split" n ss p))
printPRIM depth p@(PrimAct id@(ID n) act_t spcs) = 
    ask >>= (\opt -> let term = if act_t == OptSubProc then "}" else ";" 
                           in if gopt_prunedepth opt > 0 then
                         if depth <= gopt_prunedepth opt then 
                             return $ printAct opt id spcs term
                         else if gopt_expand opt == n then return $ printAct opt id spcs term
                              else return [""]
                     else return $ printAct opt id spcs term)

printAct :: GraphOptions -> ID -> [SPEC] -> String -> [String]
printAct opt id spcs term = if (gopt_graphtype opt) == Swimlanes 
                            then printActSwimlane (gopt_color opt) (gopt_textwidth opt) (gopt_scriptwords opt) id spcs term
                            else printActPlain (gopt_color opt) (gopt_textwidth opt) (gopt_scriptwords opt) id spcs term

printActPlain :: [(String, String)] -> Int -> Int -> ID -> [SPEC] -> String -> [String]
printActPlain c w wds id spcs term = [(printColor c (printID id)) ++ formatString' w (printActName w wds id (printScript spcs) term)]
              
printActSwimlane :: [(String, String)] -> Int -> Int -> ID -> [SPEC] -> String -> [String]
printActSwimlane c w wds id spcs term =
    let as = printAgentsSPECs spcs
        a = if length as > 0 then head as else "(none)"
    in if length as > 1         -- add a UML note with other agent names.
       then [printSwimlane a, (printColor c $ printID id) ++ printActName w wds id (printScript spcs) "", "==agents==", 
                               formatString' w  (intercalate ", " $ map trim as), term]
       else [printSwimlane a, (printColor c $ printID id) ++ printActName w wds id (printScript spcs) "", "==agent==", a, term]


-- Recursively print actions in swimlanes for each agent.
printActSwimlane' :: Int -> ID -> [String] -> [String]
printActSwimlane' _ _ [] = [""]
printActSwimlane' w id (a:as) = [a ++ ", "] ++ printActSwimlane' w id as

printActName :: Int -> Int -> ID -> String -> String -> String
--printActName w wds id script term = ":" ++ (formatString w $ printID id ++ (if wds > 0 then " - " ++ (unwords $ take wds $ words $ script) else "") ) ++ term
printActName w wds id script term = ":" ++ (formatString' w $ printID id ++ (if wds > 0 then " - " ++ (unwords $ take wds $ words $ script) else "") ) ++ term

printPseudoAct :: GraphOptions -> OPTNM -> [String]
printPseudoAct opt n = printAct opt (ID $ printOPTNM n) [] ";"


printPRIM' :: Int -> String -> String -> OPTNM -> [String] -> PRIM -> (Reader GraphOptions) [String]
printPRIM' depth pre post n ss p = 
    optPruneDepth >>= (\d -> if d > 0 then 
                                 if depth < d then 
                                     printPRIM'' pre post n ss p
                                 else if depth >= d then ask >>= 
                                          (\opt -> if shouldExpand opt n then printPRIM'' pre post n ss p
                                                   else if depth == d then return $ printPseudoAct opt n
                                                        else return [""])
                                      else printPRIM'' pre post n ss p
                             else printPRIM'' pre post n ss p)

printPRIM'' :: String -> String -> OPTNM -> [String] -> PRIM -> (Reader GraphOptions) [String]
printPRIM'' pre post id ss p = 
    ask >>= (\opt -> let t = gopt_graphtype opt in 
                     return $ [swimlanePRIMs t [p], printPartition opt id, pre] ++ ss ++ [swimlanePRIMs t [p], post, printPartitionEnd t id])

printSelect :: Int -> [PRIM] -> (Reader GraphOptions) [String]
printSelect _ [] = return []
printSelect depth (p:r) 
  | length r == 0 = printPRIM depth p 
  | length r == 1 = sequence [printPRIM depth p, printSelect depth r] >>= (\ss -> return $ intercalate ["else (or)"] ss) 
  | otherwise = sequence [printPRIM depth p, printSelect depth r] >>= (\ss -> return $ intercalate ["elseif () then (or)"] ss) 

printPartition :: GraphOptions ->  OPTNM -> String
printPartition _ OpNmNull =  ""
printPartition opt (OpNmId n) = case gopt_graphtype opt of 
                                  Swimlanes -> ""
                                  otherwise -> "partition " ++ printColor (gopt_color opt) (printID n) ++ " \"" ++ (formatString (gopt_textwidth opt) $ printID n) ++ "\" {"

printPartitionEnd :: GraphType -> OPTNM -> String
printPartitionEnd _ OpNmNull = ""
printPartitionEnd Swimlanes (OpNmId n) = "" -- Swimlanes 
printPartitionEnd _ (OpNmId _) = "}"

printColor :: [(String, String)] -> String -> String
--printColor colors name = case lookup name colors of 
printColor colors name = case find (\(n, _) -> isPrefixOf n name) colors of 
                           Just (_, c) -> "#" ++ c
                           Nothing -> ""

