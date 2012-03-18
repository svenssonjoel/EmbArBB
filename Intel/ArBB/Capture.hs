{- 2012 Joel Svensson -} 

{-# LANGUAGE ScopedTypeVariables, 
             FlexibleInstances, 
             MultiParamTypeClasses #-}

module Intel.ArBB.Capture where 


import Intel.ArBB.DAG
import Intel.ArBB.Syntax
import Intel.ArBB.TypeCheck 
import Intel.ArBB.Vector 
import Intel.ArBB.Embeddable

import qualified Intel.ArbbVM as VM 
import qualified Intel.ArbbVM.Convenience as VM

import Control.Monad.State
----------------------------------------------------------------------------
-- 

class Capture a where
  capture :: a -> IO () -- VM.EmitArbb VM.ConvFunction  
  
instance EmbFun a b => Capture (a -> b) where 
  capture f = 
    do 
      let e = runState (emb f) 0 
      putStrLn$ show e 
             
              
  
-- TODO: Something like the below but that also keeps track of the types 
-- also (generate the needed input_variable_to_Type_map)
type VarGenerator a = State Integer a 
getVar = do 
  i <- get 
  put (i+1)
  return $ Variable $ "v" ++ show i 
              
class EmbFun a b where 
  emb :: (a -> b) -> VarGenerator LExp 
  
instance Embeddable a => EmbFun (Exp a) (Exp b) where 
  emb f = do 
    v <- getVar 
    let myVar = E (LVar (newLabel ()) v)
        (E e) = f myVar
    return e
    
instance (Embeddable a, EmbFun c d) => EmbFun (Exp a) (c -> d) where 
  emb f = do 
    v <- getVar
    let myVar = E (LVar (newLabel ()) v) 
    emb (f (myVar)) 
    