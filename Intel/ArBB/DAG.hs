{- 2012 Joel Svensson -} 

module Intel.ArBB.DAG where 

--import Intel.ArBB.Syntax
import Intel.ArBB.Literal
import Intel.ArBB.Variable
import Intel.ArBB.Op

import Data.Word
import Data.Int
import qualified Data.Map as Map

---------------------------------------------------------------------------- 
-- DAG
type NodeID = Integer -- Word32

data Node = NLit Literal
          | NVar Variable 
          
          | NIndex0 NodeID
          
          | NResIndex NodeID Int 
          | NCall Integer [NodeID] 
          | NMap  Integer [NodeID]
            
          | NIf NodeID NodeID NodeID
          
          | NOp Op [NodeID] 
            
          | NWhile [Variable] NodeID [NodeID] [NodeID]
            
          deriving (Eq,Show)

type DAG = Map.Map NodeID Node

