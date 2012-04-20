{- 2012 Joel Svensson -} 

{-# LANGUAGE ScopedTypeVariables, 
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             TypeFamilies,
             TypeOperators #-}

module Intel.ArBB.Capture where 


import Intel.ArBB.DAG
import Intel.ArBB.Syntax
import Intel.ArBB.TypeCheck 
import Intel.ArBB.Types
import Intel.ArBB.Vector 
import Intel.ArBB.Data -- Embeddable
import Intel.ArBB.WithArBB
import Intel.ArBB.GenArBB
import Intel.ArBB.IsScalar


import qualified Intel.ArbbVM as VM 
import qualified Intel.ArbbVM.Convenience as VM

import Control.Monad.State hiding (liftIO)
import qualified Data.Map as Map

----------------------------------------------------------------------------
-- 
capture :: EmbFun a b => (a -> b) -> ArBB (Function (InType a b) (OutType b))
capture f = 
  do  
    fn <- getFunName 
    let ((e,tins',touts),(_,vt))    = runState (emb f) (0,Map.empty) 
        (nids,dag) = accmDAGMaker e -- runDAGMaker (constructDAG e) 
        
        -- DONE: Interleave typechecking with codeGen ! so 
        -- that local variables can be added as it moves along. 
        -- tc         = typecheckDAG dag vt
        --  Now I should have all parts needed to generate the function.
    
    let (names,tins) = unzip tins'
    
    arbbIns  <- liftVM$ mapM toArBBType tins 
    arbbOuts <- liftVM$ mapM toArBBType touts
    
    (funMap,_) <- get
    fd <- liftVM$ VM.funDef_ fn (concat arbbOuts) (concat arbbIns) $ \ os is -> 
      do 
        vs <- accmBody dag nids vt funMap (zip names is) 
         -- lift$ putStrLn $ "os :" ++ show os 
         -- lift$ putStrLn $ "vs :" ++ show vs
        copyAll os vs 

    addFunction fn fd tins touts                                                         
    return $ embFun fn f            
{-     
capture2 :: (VarGen a, EmbIn a, EmbOut b) => (a -> b) -> ArBB (Function (IT a) (OT b))  
capture2 f = 
  do  
    fn <- getFunName 
    let ((e,tins',touts),(_,vt))    = runState (emb f) (0,Map.empty) 
        (nids,dag) = accmDAGMaker e -- runDAGMaker (constructDAG e) 
        
        -- DONE: Interleave typechecking with codeGen ! so 
        -- that local variables can be added as it moves along. 
        -- tc         = typecheckDAG dag vt
        --  Now I should have all parts needed to generate the function.
    
    let (names,tins) = unzip tins'
    
    arbbIns  <- liftVM$ mapM toArBBType tins 
    arbbOuts <- liftVM$ mapM toArBBType touts
    
    (funMap,_) <- get
    fd <- liftVM$ VM.funDef_ fn (concat arbbOuts) (concat arbbIns) $ \ os is -> 
      do 
        vs <- accmBody dag nids vt funMap (zip names is) 
         -- lift$ putStrLn $ "os :" ++ show os 
         -- lift$ putStrLn $ "vs :" ++ show vs
        copyAll os vs 

    addFunction fn fd tins touts                                                         
    return $ embFun fn f                
-}     

embFun :: EmbFun a b => String -> (a -> b) -> Function (InType a b) (OutType b) 
embFun name f = Function name 

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

type Outs = [Type]
type Ins = [(Variable,Type)]    
           
class EmbFun a b where 
  type InType a b
  type OutType b 
  
  emb :: (a -> b) -> VarGenerator ([LExp], Ins, Outs) 
  
-- TODO: What is a good way to make this work with many outputs
instance Data (Exp b) => EmbFun () (Exp b) where
  type InType () (Exp b)  = () 
  type OutType (Exp b) = b
  
  emb f = do 
    let exp@(E e) = f ()
        t_out = typeOf exp
    return ([e],[],[t_out])
  
   
instance (Data (Exp b), Data a) => EmbFun (Exp a) (Exp b) where 
  type InType (Exp a) (Exp b)  = a 
  type OutType (Exp b) = b
  
  emb f = do 
    v <- getVar 
    let myVar = E (LVar (newLabel ()) v)
        exp@(E e) = f myVar
        t_in = typeOf (undefined :: a) 
        t_out = typeOf exp -- (undefined :: b)
    addType v t_in
    return ([e],[(v,t_in)],[t_out]) 
    
    
 
instance (Data b, Data c, Data a) => EmbFun (Exp a) (Exp b, Exp c) where 
  type InType (Exp a) (Exp b,Exp c)  = a 
  type OutType (Exp b,Exp c) = (b :- c)
  
  emb f = do 
    v <- getVar 
    let myVar = E (LVar (newLabel ()) v)
        (e1'@(E e1),e2'@(E e2)) = f myVar
        t_in = typeOf (undefined :: a) 
        t_out1 = typeOf (undefined :: b)
        t_out2 = typeOf (undefined :: c)
        -- t_out1 = typeOf (undefined :: b,undefined :: c)
    addType v t_in
    return ([e1,e2],[(v,t_in)],[Tuple [t_out1,t_out2]])
    
instance (Data b, Data c, Data d, Data a) => EmbFun (Exp a) (Exp b, Exp c, Exp d) where 
  type InType (Exp a) (Exp b,Exp c, Exp d)  = a 
  type OutType (Exp b,Exp c, Exp d) = (b :- c :- d)
  
  emb f = do 
    v <- getVar 
    let myVar = E (LVar (newLabel ()) v)
        (e1'@(E e1),e2'@(E e2),e3'@(E e3)) = f myVar
        t_in = typeOf (undefined :: a) 
        t_out1 = typeOf (undefined :: b)
        t_out2 = typeOf (undefined :: c)
        t_out3 = typeOf (undefined :: d) 
        -- t_out1 = typeOf (undefined :: b,undefined :: c)
    addType v t_in
    return ([e1,e2,e3],[(v,t_in)],[Tuple [t_out1,t_out2,t_out3]])    
   
  
instance (Data a, EmbFun c d) => EmbFun (Exp a) (c -> d) where 
  type InType (Exp a) (c -> d) = (a :- InType c d) 
  type OutType (c -> d) = OutType d
  
  emb f = do 
    v <- getVar
    let myVar = E (LVar (newLabel ()) v) 
        t_in = typeOf (undefined :: a) 
    addType v t_in 
    (exp,ins,outs) <- emb (f (myVar)) 
    return (exp, (v,t_in):ins, outs)
  
  
  