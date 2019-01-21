{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-- convert PML to graphviz dot format
module Formal.PML.PrintDot (printDotProcess, printDotPRIM) where
import Formal.PML.GraphOptions
import Formal.PML.PrintBasic 
import Control.Monad.Reader (Reader(..), ask)
import Formal.PML.AbsPML
import Data.Char
import Data.List (nub, intercalate, intersperse, sort)
import Text.Pandoc (readMarkdown, writePlain, ReaderOptions(..), WriterOptions(..), def)
import Text.Printf


printDotProcess :: PROCESS -> (Reader GraphOptions) [String]
printDotProcess (Process id ps) = printPRIMs 0 ps >>= printDot (PrimSeq (OpNmId id) ps) ps

printDotPRIM :: PRIM  -> (Reader GraphOptions) [String]
printDotPRIM p = printDotPRIM' p

printDotPRIM' :: PRIM  -> (Reader GraphOptions) [String]
printDotPRIM' p = case p of 
                   p@(PrimSeq  n ps)  -> printPRIM 0 p >>= printDot p ps
                   p@(PrimSeln n ps)  -> printPRIM 0 p >>= printDot p ps
                   p@(PrimBr   n ps)  -> printPRIM 0 p >>= printDot p ps
                   p@(PrimIter n ps)  -> printPRIM 0 p >>= printDot p ps
                   p@(PrimTask n ps)  -> printPRIM 0 p >>= printDot p ps
                   p@(PrimAct  _ _ _) -> printPRIM 0 p >>= printDot p [p]

printDot :: PRIM -> [PRIM] -> [String] -> (Reader GraphOptions) [String]
printDot root children body = do
  t <- optGraphType
  case t of Dataflow -> return $ intersperse "\n" $ 
                               [printf "digraph %s {" (printTitle root)] ++ body ++ ["}"]
            otherwise -> undefined


printPRIMs :: Int -> [PRIM] -> (Reader GraphOptions) [String]
printPRIMs depth ps = mapM (printPRIM depth) ps >>= (\ss -> return $ concat ss)


printPRIM :: Int -> PRIM -> (Reader GraphOptions) [String]
printPRIM depth p@(PrimSeq  n ps) = (printPRIMs (depth + 1) ps)
printPRIM depth p@(PrimSeln n ps) = (printPRIMs (depth + 1) ps)
printPRIM depth p@(PrimBr   n ps) = (printPRIMs (depth + 1) ps)
printPRIM depth p@(PrimIter n ps) = (printPRIMs (depth + 1) ps)
printPRIM depth p@(PrimTask n ps) = (printPRIMs (depth + 1) ps)
printPRIM depth p@(PrimAct id act_t spcs) = 
    ask >>= (\opt -> if gopt_prunedepth opt > 0 then
                         if depth <= gopt_prunedepth opt then 
                         return $ printAct opt id spcs 
                         else return []
                     else return $ printAct opt id spcs)

printAct :: GraphOptions -> ID -> [SPEC] -> [String]
printAct opt id spcs = [printAct' opt id spcs] ++ (printRequires opt id spcs) ++ (printProvides opt id spcs)

printRequires  :: GraphOptions -> ID -> [SPEC] -> [String]
printRequires opt id spcs = map (\r -> if length r > 0 then  r ++ " -> " ++ (printID id) ++ " ;" else []) $ printProvidesSPECs spcs 

printProvides  :: GraphOptions -> ID -> [SPEC] -> [String]
printProvides opt id spcs = map (\r -> if length r > 0 then printf "%s -> %s;" (printID id) r else []) $ printProvidesSPECs spcs 

printAct' :: GraphOptions -> ID -> [SPEC] -> String
--printAct' opt id spcs = printf "[label=\"%s - %s\"]" (printID id) (unwords $ take 5 $ words $ printScript spcs)
printAct' opt id spcs = let script = (printScript spcs) 
                        in if null script then printf "%s [label=\"%s\"];" (printID id) (printID id) else printf "%s [label=\"%s - %s\"];" (printID id) (printID id) script

