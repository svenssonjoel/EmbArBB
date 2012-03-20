{- 2012 Joel Svensson -} 

{-# LANGUAGE ScopedTypeVariables, 
             FlexibleInstances, 
             MultiParamTypeClasses #-}

module Intel.ArBB.Capture where 


import Intel.ArBB.DAG
import Intel.ArBB.Syntax
import Intel.ArBB.TypeCheck 
import Intel.ArBB.Types
import Intel.ArBB.Vector 
import Intel.ArBB.Embeddable
import Intel.ArBB.WithArBB
import Intel.ArBB.GenArBB

import qualified Intel.ArbbVM as VM 
import qualified Intel.ArbbVM.Convenience as VM

import Control.Monad.State hiding (liftIO)
import qualified Data.Map as Map

----------------------------------------------------------------------------
-- 
-- TODO: Make the returned "Function" some opaque type (not just a String) 
capture :: EmbFun a b => (a -> b) -> ArBB FunctionName 
capture f = 
  do  
    fn <- getFunName 
    let ((e,tins,touts),(_,vt))    = runState (emb f) (0,Map.empty) 
        (nid,dag) = runDAGMaker (constructDAG e) 
        tc        = typecheckDAG dag vt
        --  Now I should have all parts needed to generate the function.
    
    -- liftIO$ putStrLn$ show dag 
    -- liftIO$ putStrLn$ show tc
    -- liftIO$ putStrLn$ show tins
    -- liftIO$ putStrLn$ show touts
    arbbOuts <- liftVM$ mapM toArBBType touts
    arbbIns  <- liftVM$ mapM toArBBType tins 
    
    (funMap,_) <- get
    fd <- liftVM$ VM.funDef_ fn arbbOuts arbbIns $ \ os is -> 
      do 
        v <- genBody dag nid tc funMap is
        VM.copy_ (head os) v 

    addFunction fn fd                                                          
    return fn            
    
----------------------------------------------------------------------------
-- 
        

type VarGenerator a = State (Integer,VarType) a 
getVar :: VarGenerator Variable
getVar = do 
  (i,m) <- get 
  put (i+1,m)
  return $ Variable $ "v" ++ show i 
  
addType :: Variable -> Type -> VarGenerator ()
addType v t = 
  do 
    (i,m) <- get
    put (i,Map.insert v t m)
              
----------------------------------------------------------------------------       
-- 

type IOs = [Type]
    
class EmbFun a b where 
  emb :: (a -> b) -> VarGenerator (LExp, IOs, IOs) 
  
  
-- TODO: What is a good way to make this work with many outputs
instance (Embeddable b, Embeddable a) => EmbFun (Exp a) (Exp b) where 
  emb f = do 
    v <- getVar 
    let myVar = E (LVar (newLabel ()) v)
        (E e) = f myVar
        t_in = typeOf (undefined :: a) 
        t_out = typeOf (undefined :: b)
    addType v t_in
    return (e,[t_in],[t_out])
    
instance (Embeddable a, EmbFun c d) => EmbFun (Exp a) (c -> d) where 
  emb f = do 
    v <- getVar
    let myVar = E (LVar (newLabel ()) v) 
        t_in = typeOf (undefined :: a) 
    addType v t_in 
    (exp,ins,outs) <- emb (f (myVar)) 
    return (exp, t_in:ins, outs)
    