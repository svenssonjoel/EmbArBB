{- 2012 Joel Svensson -} 


module Intel.ArBB.GenArBB where 

import Intel.ArBB.DAG
import Intel.ArBB.Syntax 
import Intel.ArBB.TypeCheck 


import qualified Intel.ArbbVM as VM 
import qualified Intel.ArbbVM.Convenience as VM

import qualified Data.Map as Map

genBody :: DAG -> NodeID -> NodeIDType -> (Map.Map FunctionName VM.ConvFunction) ->  [VM.Variable] -> [VM.Variable] -> VM.EmitArbb ()
genBody dag nid typem funm os is = return ()