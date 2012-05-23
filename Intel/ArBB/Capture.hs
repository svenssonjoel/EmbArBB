{- 2012 Joel Svensson -} 

{-# LANGUAGE ScopedTypeVariables, 
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances,
             TypeFamilies,
             TypeOperators, 
             CPP #-}

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

-- TODO: remove this dependency
import Intel.ArBB.ExpToLExp 


import qualified Intel.ArbbVM as VM 
import qualified Intel.ArbbVM.Convenience as VM

import Control.Monad.State hiding (liftIO)
import qualified Data.Map as Map

import Data.IORef
import Data.Int
import Data.Word

----------------------------------------------------------------------------
-- 
capture :: EmbFun a b => (a -> b) -> ArBB (Function (InType a b) (OutType b))
capture f = 
  do  
    fn <- getFunName 
    let ((e,tins',touts),(_,vt))    = runState (emb f) (0,Map.empty) 
    labeled_e <- liftIO$ labelExps e
    let (nids,dag) = accmDAGMaker labeled_e -- (map expToLExp e) -- runDAGMaker (constructDAG e) 
        
        -- DONE: Interleave typechecking with codeGen ! so 
        -- that local variables can be added as it moves along. 
        -- tc         = typecheckDAG dag vt
        --  Now I should have all parts needed to generate the function.
    
    let (names,tins) = unzip tins'
    -- liftIO$ putStrLn $ "a1" 
    arbbIns  <- liftVM$ mapM toArBBType tins 
    arbbOuts <- liftVM$ mapM toArBBType touts
    -- liftIO$ putStrLn $ "a2" 
    (funMap,_) <- get
    fd <- liftVM$ VM.funDef_ fn (concat arbbOuts) (concat arbbIns) $ \ os is -> 
      do 
        -- lift$ putStrLn $ "a3" 
        vs <- accmBody dag nids vt funMap (zip names is) 
         -- lift$ putStrLn $ "os :" ++ show os 
         -- lift$ putStrLn $ "vs :" ++ show vs
        -- lift$ putStrLn $ "a4" 
        copyAll os vs 
        -- lift$ putStrLn $ "a5" 
    -- liftIO$ putStrLn $ "a6" 
    addFunction fn fd tins touts                                                         
    return $ embFun fn f            

embFun :: EmbFun a b => String -> (a -> b) -> Function (InType a b) (OutType b) 
embFun name f = Function name 


----------------------------------------------------------------------------
-- 

capture2 :: (EmbIn a b, EmbOut (a -> b), EmbF a b ) 
            => (a -> b) 
            -> ArBB (Function (EIn a b) (EOut b))
capture2 f = 
  do
    --liftIO$ putStrLn "cap1"
    fn <- getFunName 
    --liftIO$ putStrLn "cap2"
    let (e,(_,vt))  = runState (embF f) (0,Map.empty)
    labeled_e <- liftIO$ labelExps e 
    let (nids,dag)  = accmDAGMaker labeled_e -- (map expToLExp e)
        tins  = inTypes f
        touts = outTypes f

    --liftIO$ putStrLn "cap3"
        
    arbbIns <- liftVM$ mapM toArBBType tins
    arbbOuts <- liftVM$ mapM toArBBType touts 
    
    --liftIO$ putStrLn "cap4"
    (funMap,_) <- get 
    --liftIO$ putStrLn "cap5"
    fd <- liftVM$ VM.funDef_ fn (concat arbbOuts) (concat arbbIns) $ \ os is -> 
      do 
        --lift$ putStrLn "capFun 1"
        vs <- accmBody dag nids vt funMap (zip names is) 
        --lift$ putStrLn "capFun 2"
        copyAll os vs
    --liftIO$ putStrLn "cap6"
    addFunction fn fd tins touts
    --liftIO$ putStrLn "cap7"
    return (Function fn)
      where 
        names = [Variable ("v"++show i) | i <- [0..]]



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
  
  emb :: (a -> b) -> VarGenerator ([Expr], Ins, Outs) 
  
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
    let myVar = E (Var v)
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
    let myVar = E (Var v)
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
    let myVar = E (Var  v)
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
    let myVar = E (Var v) 
        t_in = typeOf (undefined :: a) 
    addType v t_in 
    (exp,ins,outs) <- emb (f (myVar)) 
    return (exp, (v,t_in):ins, outs)
  
  
---------------------------------------------------------------------------- 
-- 

class EmbOut a where 
  type EOut a 
  outTypes :: a -> [Type] 
  
class EmbIn a b where 
  type EIn a b 
  
  inTypes :: (a -> b)  -> [Type]
  

class EmbF a b where 
  embF :: (a -> b) -> VarGenerator [Expr] 
  
  
----------------------------------------------------------------------------
#define OScalar(a)                          \
  instance Data a => EmbOut (Exp a) where { \
    type EOut (Exp a) = IORef a;            \
    outTypes _ = [typeOf (undefined :: a)]}

OScalar(Int) 
OScalar(Int8) 
OScalar(Int16) 
OScalar(Int32) 
OScalar(Int64) 
OScalar(Float) 
OScalar(Double) 
OScalar(Word)
OScalar(Word8)
OScalar(Word16)
OScalar(Word32)
OScalar(Word64)


instance Data (Exp (DVector t a))  => EmbOut (Exp (DVector t a)) where 
  type EOut (Exp (DVector t a)) = MDVector t a 
  outTypes a = [typeOf a]

instance (EmbOut a, EmbOut b) => EmbOut (a,b) where 
  type EOut (a,b) = (EOut a :- EOut b) 
  outTypes (a,b) = outTypes a ++ outTypes b 

instance (EmbOut a, EmbOut b, EmbOut c) => EmbOut (a,b,c) where 
  type EOut (a,b,c) = (EOut a :- EOut b :- EOut c) 
  outTypes (a,b,c) = outTypes a ++ outTypes b ++ outTypes c 

instance EmbOut b => EmbOut (Exp a -> b) where 
  type EOut ((Exp a) -> b) = EOut b 
  outTypes f = outTypes (f nonsence) 
    where 
       nonsence = E (Var (Variable "nonsence"))
----------------------------------------------------------------------------  
#define BaseIn(rt)                               \
   instance Data a => EmbIn (Exp a) (rt) where { \
     type EIn (Exp a) (rt) = a;                  \
     inTypes a = [typeOf (undefined :: a)]     } 

BaseIn(())
BaseIn(Exp b)
BaseIn((Exp b, Exp c)) 
BaseIn((Exp b, Exp c, Exp d)) 

instance (Data  a, EmbIn b c) => EmbIn (Exp a) (b -> c) where
  type EIn (Exp a) (b -> c) = a :- EIn b c 
  inTypes f = typeOf (undefined :: a) : inTypes (f nonsence) 
    where 
      nonsence = E (Var (Variable "nonsence"))
      
----------------------------------------------------------------------------


instance  Data a => EmbF (Exp a) (Exp b) where 
  embF f = do  
    v <- getVar
    let myVar = E (Var v) 
        t_in = typeOf (undefined :: a) 
    addType v t_in 
    let (E exp) =  f myVar
    return [exp] 

instance  Data a => EmbF (Exp a) (Exp b,Exp c) where 
  embF f = do  
    v <- getVar
    let myVar = E (Var v) 
        t_in = typeOf (undefined :: a) 
    addType v t_in 
    let (E exp1,E exp2) =  f myVar
    return [exp1,exp2] 

instance  Data a => EmbF (Exp a) (Exp b,Exp c, Exp d) where 
  embF f = do  
    v <- getVar
    let myVar = E (Var v) 
        t_in = typeOf (undefined :: a) 
    addType v t_in 
    let (E exp1,E exp2,E exp3) =  f myVar
    return [exp1,exp2,exp3] 

instance (Data a, EmbF b c) => EmbF (Exp a) (b -> c) where 
  embF f = do 
    v <- getVar 
    let myVar = E (Var v) 
        t_in = typeOf (undefined :: a) 
    addType v t_in 
    embF (f myVar)
    
----------------------------------------------------------------------------