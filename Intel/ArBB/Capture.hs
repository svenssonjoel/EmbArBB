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
import Intel.ArBB.Data.Int


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

capture :: (EmbIn a b, EmbOut (a -> b), EmbF a b ) 
            => (a -> b) 
            -> ArBB (Function (EIn a b) (EOut b))
capture f = 
  do
    --liftIO$ putStrLn "cap1"
    fn <- getFunName 
    --liftIO$ putStrLn "cap2"
    let (e,(_,vt))  = runState (embF f) (0,Map.empty)
    --liftIO$ putStrLn "cap2a"    
    labeled_e <- liftIO$ labelExps e 
    --liftIO$ putStrLn "cap2b"
    let (nids,dag)  = accmDAGMaker labeled_e -- (map expToLExp e)
        tins  = inTypes f
        touts = outTypes f

    --liftIO$ putStrLn "cap3"
        
    arbbIns <- liftVM$ mapM toArBBType tins
    arbbOuts <- liftVM$ mapM toArBBType touts 
    
    --liftIO$ putStrLn "cap4"
    (funMap,_,_) <- get 
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
    type EOut (Exp a) = a;                  \
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
OScalar(USize)
OScalar(ISize)


instance Data (Exp (DVector t a))  => EmbOut (Exp (DVector t a)) where 
  type EOut (Exp (DVector t a)) = DVector t a 
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