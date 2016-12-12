module Formal.PML.GraphOptions where
import Control.Monad.Reader (Reader(..), ask)

-- Tramp data to be passed around various print modules.
data GraphType = Partitions | Swimlanes | Agents | Requires | Provides | Dataflow | Controlflow
  deriving (Eq, Show)

data GraphOptions = GOpt {
      gopt_color :: [(String, String)]
    , gopt_graphtype :: GraphType
    , gopt_prunedepth :: Int
    , gopt_expand :: String
    , gopt_textwidth :: Int     -- "fill" column (width of action label in characters)
    , gopt_scriptwords :: Int   -- number of words from script to print
}

defGraphOptions =  GOpt {
                     gopt_graphtype = Partitions
                   , gopt_prunedepth = 0
                   , gopt_expand = []
                   , gopt_color = [] 
                   , gopt_textwidth = 20
                   , gopt_scriptwords = 10
}

optGraphType :: (Reader GraphOptions) GraphType
optGraphType = ask >>= (\gopts -> return $ gopt_graphtype gopts)

optPruneDepth :: (Reader GraphOptions) Int
optPruneDepth = ask >>= (\gopts -> return $ gopt_prunedepth gopts)

