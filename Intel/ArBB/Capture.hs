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

import qualified Intel.ArbbVM as VM 
import qualified Intel.ArbbVM.Convenience as VM

import Control.Monad.State hiding (liftIO)
import qualified Data.Map as Map

----------------------------------------------------------------------------
-- 

class Capture a where
  capture :: a -> ArBB FunctionName -- VM.EmitArbb VM.ConvFunction  
  
instance EmbFun a b => Capture (a -> b) where 
  capture f = 
    do  
      fn <- getFunName 
      let e = runState (emb f) (0,Map.empty) 
      liftIO$ putStrLn$ show e 
      fd <- liftVM$ VM.funDef_ fn [] [] $ \ x y -> do return () 
      addFunction fn fd                                                          
      return fn                                                         
      
             
              
  
-- TODO: Something like the below but that also keeps track of the types 
-- also (generate the needed input_variable_to_Type_map)
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
              
class EmbFun a b where 
  emb :: (a -> b) -> VarGenerator LExp 
  
instance Embeddable a => EmbFun (Exp a) (Exp b) where 
  emb f = do 
    v <- getVar 
    let myVar = E (LVar (newLabel ()) v)
        (E e) = f myVar
        t = typeOf (undefined :: a) 
    addType v t
    return e
    
instance (Embeddable a, EmbFun c d) => EmbFun (Exp a) (c -> d) where 
  emb f = do 
    v <- getVar
    let myVar = E (LVar (newLabel ()) v) 
        t = typeOf (undefined :: a) 
    addType v t 
    emb (f (myVar)) 
    