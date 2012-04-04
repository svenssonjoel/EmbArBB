{- 2012 Joel Svensson -} 

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
          
          | NIndex0 NodeID
          
          | NResIndex NodeID Int 
          | NCall FunctionName [NodeID] 
          | NMap  FunctionName [NodeID]
            
          | NIf NodeID NodeID NodeID
            -- experimental 
          | NOp Op [NodeID] 
            
            --Hmm dont know about this... 
          | NFor NodeID [NodeID]   
            
          deriving (Eq,Show)

type DAG = Map.Map NodeID Node

---------------------------------------------------------------------------- 
-- very sketchy things 

type DAGMaker a = State DAG a  
             
runDAGMaker lexpr = runState lexpr Map.empty

accmDAGMaker xs = doAccm xs Map.empty 
  where 
    doAccm [] m = ([],m)
    doAccm (x:xs) m = let (x',m'') = runState (constructDAG x) m'
                          (xs',m') = doAccm xs m
                      in (x':xs',m'')
      
    
----------------------------------------------------------------------------
-- construct DAG
-- TODO: Clean up and make less CutNPasteish 
constructDAG :: LExp -> DAGMaker NodeID 
constructDAG (LVar l v) = 
  do 
    m <- get 
    let m' = Map.insert l (NVar v) m 
    put m'
    return l 
constructDAG (LLit l i) = 
  do 
    m <- get 
    let m' = Map.insert l (NLit i) m 
    put m'
    return l

constructDAG (LResIndex l e i) = do 
  m <- get 
  case Map.lookup l m of 
    (Just nid) -> return l 
    Nothing -> 
      do 
        e' <- constructDAG e 
        m' <- get
        let m'' = Map.insert l (NResIndex e' i) m' 
        put m''
        return l    
 
constructDAG (LIndex0 l e) = do 
  m <- get 
  case Map.lookup l m of 
    (Just nid) -> return l 
    Nothing -> 
      do 
        e' <- constructDAG e 
        m' <- get
        let m'' = Map.insert l (NIndex0 e') m' 
        put m''
        return l   
        
constructDAG (LIf l e1 e2 e3) = do 
  m <- get 
  case Map.lookup l m of 
    (Just nid) -> return l
    Nothing -> 
      do
        e1' <- constructDAG e1 
        e2' <- constructDAG e2 
        e3' <- constructDAG e3 
        m' <- get
        let m'' = Map.insert l (NIf e1' e2' e3') m'
        put m''
        return l
        
constructDAG (LOp l op es)  = 
  do       
    m <- get 
    case Map.lookup l m of 
      (Just nid) -> return l 
      Nothing -> 
        do 
          es' <- constrAll es 
          m' <- get 
          let m'' = Map.insert l (NOp op es') m'
          put m''     
          return l 
constructDAG (LFor l cond body state1) = 
  do 
    m <- get 
    case Map.lookup l m of 
      (Just nid) -> return l 
      Nothing ->  undefined  
constructDAG x = error $ show x 
          
constrAll [] = return []
constrAll (x:xs) = 
  do 
    x' <- constructDAG x 
    xs' <- constrAll xs 
    return (x':xs')
    
