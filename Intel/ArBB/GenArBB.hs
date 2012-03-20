{- 2012 Joel Svensson -} 


module Intel.ArBB.GenArBB where 

import Intel.ArBB.DAG
import Intel.ArBB.Syntax 
import Intel.ArBB.TypeCheck 
import Intel.ArBB.Types


import qualified Intel.ArbbVM as VM 
import qualified Intel.ArbbVM.Convenience as VM

import qualified Data.Map as Map
import Control.Monad.State
---------------------------------------------------------------------------- 
-- 
type Gen a = StateT (Map.Map NodeID VM.Variable) VM.EmitArbb a


----------------------------------------------------------------------------
-- Wrapper 
genBody :: DAG 
           -> NodeID 
           -> NodeIDType 
           -> (Map.Map FunctionName VM.ConvFunction) 
           ->  [VM.Variable] 
           -> [VM.Variable] 
           -> VM.EmitArbb () -- will be VM.Variable
genBody dag nid typem funm os is = evalStateT (genBody' dag nid typem funm os is) (Map.empty) 

----------------------------------------------------------------------------
-- Helpers 
visited :: NodeID -> Gen (Maybe VM.Variable) 
visited nid = 
  do 
    m <- get 
    return$ Map.lookup nid m 

addNode :: NodeID -> VM.Variable -> Gen () 
addNode nid v = 
  do 
    m <- get 
    let m' = Map.insert nid v m 
    put m' 
    

----------------------------------------------------------------------------
-- The real worker
genBody' :: DAG 
           -> NodeID 
           -> NodeIDType 
           -> (Map.Map FunctionName VM.ConvFunction) 
           ->  [VM.Variable] 
           -> [VM.Variable] 
           -> Gen () -- will be VM.Variable
genBody' dag nid typem funm os is = 
  do 
    case Map.lookup nid dag of 
      Nothing -> error "genBody: DAG is broken" 
      (Just node) -> genNode node 
  where 
    genNode :: Node -> Gen ()
    genNode _ = return ()  
    
    

----------------------------------------------------------------------------   
-- 
        
    
toArBBType (Scalar t) = VM.getScalarType_ t
toArBBType (Dense I t) = 
  do 
    st <- VM.getScalarType_ t
    VM.getDenseType_ st 1 
toArBBType (Dense II t) = 
  do 
    st <- VM.getScalarType_ t
    VM.getDenseType_ st 2 
toArBBType (Dense III t) = 
  do 
    st <- VM.getScalarType_ t
    VM.getDenseType_ st 3


    
    
