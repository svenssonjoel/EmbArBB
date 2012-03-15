

module Intel.ArBB.TypeCheck where 


{- Typecheck a DAG. and create a nodeID_to_type map -} 


import Intel.ArBB.DAG

import qualified Intel.ArbbVM as ArBB

import Control.Monad.State 
import qualified Data.Map as Map 


---------------------------------------------------------------------------- 
-- What types can we have ? 
data Type = Scalar ArBB.ScalarType 
          | Dense Dimensionality ArBB.ScalarType 

data Dimensionality = I | II | III 




---------------------------------------------------------------------------- 
-- Typechecking state
type TypeMap      = Map.Map NodeID Type 
type CheckState a = State TypeMap a 

-- Before running the typechecker the state should be initialized 
-- with the types of any inputs 


-- find all types 
typecheck :: DAG -> TypeMap -> TypeMap 
typecheck = undefined 

-- helper 
typecheckNode :: DAG -> NodeID -> TypeMap -> TypeMap 
typecheckNode = undefined 