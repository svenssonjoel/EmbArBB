{-# LANGUAGE MultiParamTypeClasses, 
             ScopedTypeVariables, 
             FlexibleInstances #-}

-- Reification (To make something concrete) 
module Intel.ArBB.Reify where 


import System.Mem.StableName 

import Control.Monad.State 
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.Dynamic as Dynamic hiding (typeOf)  

import Intel.ArBB.DAG
import Intel.ArBB.TypeCheck
import Intel.ArBB.Types 
import Intel.ArBB.Syntax
import Intel.ArBB.Data

-- Going to try to skip the step via "LExp"  before the codegeneration

-- TODO: reifyExp must also return a nodeID pointing out what node is carrying the result.

data RState = RState { sharing :: IntMap.IntMap [(Dynamic, Expr)] 
                                                 -- Dynamic part holds StableName for 
                                                 -- comparison at those cases when the 
                                                 -- returned hash Int collides 
                     , types :: VarType -- Variable to type map
                     , uniq  :: Int
                     , dag   :: DAG 
                     }

type Reifyer a = StateT RState IO a

runReifyer r = evalStateT r noRState
    where 
      noRState = 
          RState IntMap.empty
                 Map.empty
                 0 
                 Map.empty

---------------------------------------------------------------------------- 
-- 
newVar :: Reifyer Variable 
newVar = do 
  id <- gets uniq 
  modify $ \s -> s { uniq = id + 1 } 
  return $ Variable ("v" ++ show id)

addVarType :: Variable -> Type -> Reifyer () 
addVarType v t = do 
  m <- gets types 
  modify $ \s -> s { types = Map.insert v t m } 


  
----------------------------------------------------------------------------
-- Reifyers..
class Reify a where 
    reify :: a -> IO DAG
    

instance ReifyableFun a b => Reify (a -> b) where 
    reify f = runReifyer (reifyfun f) 

class ReifyableFun a b where 
    reifyfun :: (a -> b) -> Reifyer DAG -- [Expr]

instance Data a => ReifyableFun (Exp a) (Exp b) where 
    reifyfun f =  
        do
          v <- newVar 
          let t = typeOf (undefined :: a) 
          addVarType v t 
          let e = f $ E (Var v)  
                     
          reifyExpr (unE e)


---------------------------------------------------------------------------- 
-- Base reifying 

reifyExpr :: Expr -> Reifyer DAG 
reifyExpr = undefined  



{- 
---------------------------------------------------------------------------- 
-- Apply 

class Apply a b where 
    apply :: (a -> b) -> Reifyer [Expr]


instance (Data a, Data b) => Apply (Exp a) (Exp b) where 
    apply f = 
        do
          v <- newVar 
          let t = typeOf (undefined :: a) 
          addVarType v t 
          let e = f (E (Var v)) 
          return [unE e]

instance (Data a, Data b, Data c) => Apply (Exp a) (Exp b,Exp c) where 
    apply f = 
        do
          v <- newVar 
          let t = typeOf (undefined :: a) 
          addVarType v t 
          let (e1,e2) = f (E (Var v)) 
          return [unE e1,unE e2]

instance (Data a, Data b, Data c, Data d) => Apply (Exp a) (Exp b,Exp c,Exp d) where 
    apply f = 
        do
          v <- newVar 
          let t = typeOf (undefined :: a) 
          addVarType v t 
          let (e1,e2,e3) = f (E (Var v)) 
          return [unE e1,unE e2, unE e3]
           
instance (Data a, Apply b c ) => Apply (Exp a) (b -> c) where   
    apply f = 
        do 
          v <- newVar 
          let t = typeOf (undefined :: a) 
          addVarType v t 
          apply $ f (E (Var v))
-} 
----------------------------------------------------------------------------

