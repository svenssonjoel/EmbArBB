{- 2012 Joel Svensson -} 


module Intel.ArBB.GenArBB where 

import Intel.ArBB.DAG
import Intel.ArBB.Syntax 
import Intel.ArBB.TypeCheck 
import Intel.ArBB.Types
import Intel.ArBB.Data.Int 


import qualified Intel.ArbbVM as VM 
import qualified Intel.ArbbVM.Convenience as VM

import qualified Data.Map as Map
import qualified Data.List as L
import Data.Maybe 

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
           -> [VM.Variable] 
           -> VM.EmitArbb VM.Variable 
genBody dag nid typem funm is = evalStateT (genBody' dag nid typem funm is) (Map.empty) 

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
    


getTypeOfNode :: NodeID -> NodeIDType -> Gen VM.Type 
getTypeOfNode n m = 
  let t = fromJust$ Map.lookup n m 
  in 
   lift $ toArBBType t 


----------------------------------------------------------------------------
-- The real worker
genBody' :: DAG 
           -> NodeID 
           -> NodeIDType 
           -> (Map.Map FunctionName VM.ConvFunction) 
           -> [VM.Variable] 
           -> Gen VM.Variable 
genBody' dag nid typem funm is = 
  do 
    m <- get 
    case Map.lookup nid m of 
      (Just v) -> return v 
      Nothing  -> 
          case Map.lookup nid dag of 
            (Just node) -> genNode nid node 
            Nothing -> error "genBody: DAG is broken" 
            
  where 
    genNode :: NodeID -> Node -> Gen VM.Variable
    genNode thisNid (NLit l) = genLiteral l 
    genNode thisNid (NVar (Variable nom)) = 
      do 
        lift$ VM.liftIO$ putStrLn "Var node" 
        return$ is !! (read (nom L.\\ "v") :: Int)  -- inputs
    
    genNode thisNid (NReduce Add nid) = 
      do
        
        t <- getTypeOfNode thisNid typem
        v1 <- genBody' dag nid typem funm is 
        
        imm <- lift$ VM.createLocal_ t "imm"   -- st "res" 
        lift$ VM.opDynamic_ VM.ArbbOpAddReduce [imm] [v1] 
        
        -- memoize the computed var
        addNode thisNid imm 
        
        lift$ VM.liftIO$ putStrLn "NReduce Add node" 
        return imm
    genNode thisNid (NBinOp op n1 n2) = 
      do 
        v1 <- genBody' dag n1 typem funm is 
        v2 <- genBody' dag n2 typem funm is 
        
        t <- getTypeOfNode thisNid typem
        
        imm <- lift $ VM.createLocal_ t "imm" 
        lift$ VM.op_ (opToArBB op) [imm] [v1,v2] 
        
        -- memoize the computed var
        addNode thisNid imm 
        
        lift$ VM.liftIO$ putStrLn "BinOp node" 
        return imm
    genNode thisNid (NRotate n1 n2) = 
      do 
        v1 <- genBody' dag n1 typem funm is 
        v2 <- genBody' dag n2 typem funm is 
        
        t <- getTypeOfNode thisNid typem 
        
        imm <- lift$ VM.createLocal_ t "imm" 
        lift$ VM.opDynamic_ VM.ArbbOpRotate [imm] [v1,v2]
        
        addNode thisNid imm 
        lift$ VM.liftIO$ putStrLn "Rotate node" 
        return imm
    genNode thisNid (NRotateRev n1 n2) = 
      do 
        v1 <- genBody' dag n1 typem funm is 
        v2 <- genBody' dag n2 typem funm is 
        
        t <- getTypeOfNode thisNid typem 
        
        imm <- lift$ VM.createLocal_ t "imm" 
        lift$ VM.opDynamic_ VM.ArbbOpRotateReverse [imm] [v1,v2]
        
        addNode thisNid imm 
        lift$ VM.liftIO$ putStrLn "RotateRev node" 
        return imm
    

        
genLiteral :: Literal -> Gen VM.Variable
genLiteral (LitInt8 i)  = lift$ VM.int8_ i 
genLiteral (LitInt16 i) = lift$ VM.int16_ i 
genLiteral (LitInt32 i) = lift$ VM.int32_ i 
genLiteral (LitWord32 i) = lift$ VM.uint32_ i 
genLiteral (LitFloat i) = lift$ VM.float32_ i 
genLiteral (LitDouble i) = lift$ VM.float64_ i 
genLiteral (LitISize (ISize i)) = lift$ VM.isize_ i
genLiteral (LitUSize (USize i)) = lift$ VM.usize_ i



opToArBB Add = VM.ArbbOpAdd
opToArBB Mul = VM.ArbbOpMul 
opToArBB Sub = VM.ArbbOpSub

    

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


    
    
