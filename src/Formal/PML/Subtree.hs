{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-- select subtree from a PML model
module Formal.PML.Subtree where
import Formal.PML.AbsPML
import Formal.PML.ErrM
import Formal.PML.Print
import Formal.PML.PrintBasic (printOPTNM)
import Data.Char
import Data.List (intercalate, intersperse, isPrefixOf)
import Data.Maybe (fromMaybe)



-- Convenience function to parse string of pml code, then select given subtree from result.
getSubtree :: String -> String -> PROCESS
getSubtree subtree code = case parseContents code of
           Bad s    -> (Process (ID "error: ") [(PrimAct (ID "msg") OptNull [(SpecScript (STRING s))])])

           Ok  tree -> selectSubtree subtree tree


-- select subtree; return whole tree if not found
selectSubtree :: String -> PROCESS -> PROCESS
selectSubtree subtree tree@(Process (ID id) _) = 
    case selectSubtree' subtree tree of
      Just s@(Process (ID id') _) -> s
      Nothing -> tree

-- select subtree; return NOTHING if not found
selectSubtree' :: String -> PROCESS -> Maybe PROCESS
selectSubtree' subtree tree = case selectSubtree'' subtree tree of
                               Just st@(PrimBr id _)    -> Just (Process (ID $ printOPTNM id) [st])
                               Just st@(PrimSeln id _)  -> Just (Process (ID $ printOPTNM id) [st])
                               Just st@(PrimIter id _)  -> Just (Process (ID $ printOPTNM id) [st])
                               Just st@(PrimSeq id _)   -> Just (Process (ID $ printOPTNM id) [st])
                               Just st@(PrimTask id _)  -> Just (Process (ID $ printOPTNM id) [st])
                               Just st@(PrimAct id _ _) -> Just (Process id [st])
                               otherwise -> Nothing

-- select subtree; return PML construct at root of subtree, or NOTHING if not found
selectSubtree'' :: String -> PROCESS -> Maybe PRIM
selectSubtree'' subtree  (Process (ID id) ps) =
    if isPrefixOf subtree id then Just (PrimSeq (OpNmId (ID id)) ps)
    else selectSubtreePRIMs subtree ps

selectSubtreePRIMs :: String -> [PRIM] -> Maybe PRIM
selectSubtreePRIMs subtree [] = Nothing -- Just $ PrimSeq (OpNmId (ID (subtree ++ " not found"))) []
selectSubtreePRIMs subtree (p:ps) = 
    case selectSubtreePRIM subtree p of 
      Just p' -> Just p'
      otherwise -> selectSubtreePRIMs subtree ps

selectSubtreePRIM :: String -> PRIM -> Maybe PRIM
selectSubtreePRIM subtree p@(PrimBr   nm ps)  = selectSubtreePRIM' subtree nm p ps
selectSubtreePRIM subtree p@(PrimSeln nm ps)  = selectSubtreePRIM' subtree nm p ps
selectSubtreePRIM subtree p@(PrimIter nm ps)  = selectSubtreePRIM' subtree nm p ps
selectSubtreePRIM subtree p@(PrimSeq  nm ps)  = selectSubtreePRIM' subtree nm p ps
selectSubtreePRIM subtree p@(PrimTask nm ps)  = selectSubtreePRIM' subtree nm p ps
selectSubtreePRIM subtree p@(PrimAct  (ID id) _ _)          = if subtree == id then Just p else Nothing

selectSubtreePRIM' :: String -> OPTNM -> PRIM -> [PRIM] -> Maybe PRIM
selectSubtreePRIM' subtree (OpNmId (ID id)) p ps = if isPrefixOf subtree id then Just p else selectSubtreePRIMs subtree ps
selectSubtreePRIM' subtree (OpNmNull) _ ps = selectSubtreePRIMs subtree ps
