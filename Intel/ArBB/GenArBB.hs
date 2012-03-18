{- 2012 Joel Svensson -} 


module Intel.ArBB.GenArBB where 

import Intel.ArBB.DAG
import Intel.ArBB.Syntax 
import Intel.ArBB.TypeCheck 

import qualified Intel.ArbbVM as VM 
import qualified Intel.ArbbVM.Convenience as VM

import qualified Data.Map as Map

genFun :: DAG -> NodeID -> NodeIDType -> VM.EmitArbb VM.ConvFunction
genFun = undefined 


genBody :: DAG -> NodeID -> NodeIDType -> (Map.Map Variable VM.Variable) -> VM.EmitArbb VM.ConvFunction
genBody = undefined 