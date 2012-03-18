{- 2012 Joel Svensson -} 


module Intel.ArBB.GenArBB where 

import Intel.ArBB.DAG
import Intel.ArBB.Syntax 
import Intel.ArBB.TypeCheck 

import qualified Intel.ArbbVM as VM 



genFun :: DAG -> NodeID -> NodeIDType -> VM.EmitArBB VM.ConvFunction
genFun = undefined 


genBody :: DAG -> NodeID -> NodeIDType -> (Map Variable VM.Variable) -> VM.EmitArBB VM.ConvFunction
genBody = undefined 