{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-- convert PML to PlantUML Activity Diagram
module Formal.PML.Subtree where
import Formal.PML.AbsPML
import Data.Char
import Data.List (intercalate, intersperse)
import Data.Maybe (fromMaybe)

class PML a where
  slct :: String -> a -> Maybe PRIM

instance PML PROCESS where
  slct root p = case p of p@(Process id ps) -> selectSubtree' root p

selectSubtree :: PML p => String -> p -> Maybe PRIM
selectSubtree = slct

selectSubtree' :: String -> PROCESS -> Maybe PRIM
selectSubtree' root  (Process id ps) =
    if (ID root) ==  id then Just (PrimSeq (OpNmId id) ps)
    else selectSubtreePRIMs root ps

selectSubtreePRIMs :: String -> [PRIM] -> Maybe PRIM
selectSubtreePRIMs root [] = Nothing -- Just $ PrimSeq (OpNmId (ID (root ++ " not found"))) []
selectSubtreePRIMs root (p:ps) = 
    case selectSubtreePRIM root p of 
      Just p' -> Just p'
      otherwise -> selectSubtreePRIMs root ps

selectSubtreePRIM :: String -> PRIM -> Maybe PRIM
selectSubtreePRIM root p@(PrimBr   (OpNmId (ID id)) ps)  = if root == id then Just p else selectSubtreePRIMs root ps
selectSubtreePRIM root p@(PrimSeln (OpNmId (ID id)) ps)  = if root == id then Just p else selectSubtreePRIMs root ps
selectSubtreePRIM root p@(PrimIter (OpNmId (ID id)) ps)  = if root == id then Just p else selectSubtreePRIMs root ps
selectSubtreePRIM root p@(PrimSeq  (OpNmId (ID id)) ps)  = if root == id then Just p else selectSubtreePRIMs root ps
selectSubtreePRIM root p@(PrimTask (OpNmId (ID id)) ps)  = if root == id then Just p else selectSubtreePRIMs root ps
selectSubtreePRIM root p@(PrimAct  (ID id) _ _)          = if root == id then Just p else Nothing
