

module Intel.ArBB.DAG where 

import Intel.ArBB.Syntax

import Intel.ArBB.Vector -- Should not really be needed here (?)

import Data.Word
import Data.Int
import qualified Data.Map as Map

import Control.Monad.State

---------------------------------------------------------------------------- 
-- DAG
type NodeID = Word32

data Node = NLit Literal
          | NVar Variable 
          | NBinOp Op NodeID NodeID 
          | NUnOp  Op NodeID 
            
          | NIndex0 NodeID
          | NIndex1 NodeID NodeID 
            
          | NAddReduce NodeID
          | NMulReduce NodeID 
          
          | NRotate NodeID NodeID 
          | NRotateRev NodeID NodeID 
          deriving (Eq,Show)

type DAG = Map.Map NodeID Node

---------------------------------------------------------------------------- 
-- very sketchy things 

type DAGMaker a = State DAG a  
             
runDAGMaker lexpr = runState lexpr Map.empty
               
compile c = runDAGMaker (compileTest c)              

compileTest :: (Exp (Vector Int32) -> Exp (Vector0D Int32)) -> DAGMaker NodeID
compileTest f = constructDAG res 
  where 
    (E res) = f input
    input = E (LVar (newLabel ()) (Variable "Input"))
    
constructDAG :: LExp -> DAGMaker NodeID 
constructDAG (LVar l v) = 
  do 
    m <- get 
    let m' = Map.insert l (NVar v) m 
    put m'
    return l 
constructDAG (LReduce l Add input) = do     
  m <- get 
  case Map.lookup l m  of 
    (Just nid) -> return l -- correct now ? 
    Nothing    -> 
      do 
        input' <- constructDAG input 
        m' <- get 
        let m'' = Map.insert l (NAddReduce input') m'
        put m''
        return l
constructDAG (LBinOp l op i1 i2) = do        
  m <- get 
  case Map.lookup l m of 
    (Just nid) -> return l
    Nothing -> 
      do 
        i1' <- constructDAG i1 
        i2' <- constructDAG i2 
        m' <- get 
        let m'' = Map.insert l (NBinOp op i1' i2') m'
        put m'' 
        return l
      