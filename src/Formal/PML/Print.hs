module Formal.PML.Print where
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

printPML :: GraphOptions -> PROCESS -> [String]
printPML opts p = case gopt_graphtype opts of 
                    Agents -> printAgents p
                    Requires -> printRequires p
                    Provides -> printProvides p
                    Dataflow -> [printDot opts p]
                    Controlflow -> [printDot opts p]
                    otherwise -> [printUML opts p]


-- Convenience functions for printing
printUML :: GraphOptions -> PROCESS -> String
printUML gopts p = runReader (printUMLProcess p) gopts

printAgents :: PROCESS -> [String]
printAgents p@(Process id ps) = sort $ nub $   findAgentsPRIMs ps

printRequires :: PROCESS -> [String]
printRequires p@(Process id ps) = sort $ nub $ findRequiresPRIMs ps

printProvides :: PROCESS -> [String]
printProvides p@(Process id ps) = sort $ nub $ findProvidesPRIMs ps

printDot :: GraphOptions -> PROCESS -> String
printDot gopts p = intercalate "\n" $ runReader (printDotProcess p) gopts 

