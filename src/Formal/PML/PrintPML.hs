module Formal.PML.PrintPML where
import Formal.PML.AbsPML
import Formal.PML.LexPML
import Formal.PML.ParPML
import Formal.PML.SkelPML
import Formal.PML.ErrM
import Control.Monad.Reader (Reader(..), ask)
import Formal.PML.GraphOptions
import Formal.PML.PrintBasic
import Formal.PML.PrintUML
import Formal.PML.PrintDot


import Control.Monad.Reader (runReader)
import Data.List (intercalate, intersperse, nub, sort)

-- Stuff to deal with pml-bnfc
type ParseFun a = [Token] -> Err a

myLLexer = myLexer

parseContents :: String -> Err PROCESS
parseContents s = let ts = myLLexer s in pPROCESS ts 



class Print a where
    prtUML :: GraphOptions -> a -> [String]
    prtAgents :: a -> [String]
    prtRequires :: a -> [String]
    prtProvides :: a -> [String]
    prtDot :: GraphOptions -> a -> [String]

instance Print PROCESS where
    prtUML gopts p = runReader (printUMLProcess p) gopts
    prtAgents p@(Process id ps) = sort $ nub $   findAgentsPRIMs ps
    prtRequires p@(Process id ps) = sort $ nub $ findRequiresPRIMs ps
    prtProvides p@(Process id ps) = sort $ nub $ findProvidesPRIMs ps
    prtDot gopts p = runReader (printDotProcess p) gopts 

instance Print PRIM where
    prtUML gopts p = runReader (printUMLPRIM p) gopts
    prtAgents p = sort $ nub $ findAgentsPRIMs [p]
    prtRequires p = sort $ nub $ findRequiresPRIMs [p]
    prtProvides p = sort $ nub $ findProvidesPRIMs [p]
    prtDot gopts p = runReader (printDotPRIM p) gopts
