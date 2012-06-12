{-# LANGUAGE GeneralizedNewtypeDeriving, 
             MultiParamTypeClasses, 
             FlexibleInstances, 
             ScopedTypeVariables,
             TypeOperators #-}

module Intel.ArBB.BackendExperiment where 

import Control.Monad.State.Strict
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.Dynamic as Dynamic hiding (typeOf) 

import System.Mem.StableName


import Intel.ArBB.DAG
import Intel.ArBB.Types 
import Intel.ArBB.Syntax
import Intel.ArBB.Variable
import Intel.ArBB.Data
import Intel.ArBB.Vector
import Intel.ArBB.ReifyableType
import Intel.ArBB.MonadCapture
 
import Data.Int
import Data.Word
import Intel.ArBB.Data.Int

----------------------------------------------------------------------------
-- The Capture Monad Stuff 
----------------------------------------------------------------------------


---------------------------------------------------------------------------- 
-- Implement operations on the Capture monad. 
-- These should all be backend oblivious. 
newVar :: Capture Variable 
newVar = do 
  id <- gets unique 
  modify $ \s -> s { unique = id + 1 } 
  return $ Variable ("v" ++ show id)

addVarType :: Variable -> Type -> Capture () 
addVarType v t = do 
  m <- gets types 
  modify $ \s -> s { types = Map.insert v t m } 

-- TODO: Break up into tiny functions. 
-- TODO: I dont really need the Dynamic right ?
getNodeID :: Expr -> Capture NodeID
getNodeID e = 
    do
      sh <- gets sharing  
      sn <- liftIO$ makeStableName e 
      let hsn = hashStableName sn 
            
      case IntMap.lookup hsn sh of 
        (Just hits) -> case lookup (Just sn) [(fromDynamic d,i)| (d,i) <- hits] of 
                         (Just n) -> return n -- Already computed
                         Nothing  -> 
                             do 
                               uniq <- gets unique 
                               modify $ \s -> s { sharing = IntMap.insert hsn ((toDyn sn,uniq):hits) sh} 
                               modify $ \s -> s { unique = uniq + 1}
                               return uniq
        Nothing     -> 
            do
              uniq <- gets unique
              modify $ \s -> s {sharing = IntMap.insert hsn [(toDyn sn,uniq)] sh}
              modify $ \s -> s {unique = uniq + 1}
              return uniq


insertNode :: Expr -> Node -> Capture [NodeID]
insertNode e node = 
    do 
      d <- gets dag
      nid <- getNodeID e 
      let d' = Map.insert nid node d 
      modify $ \s -> s {dag = d'} 
      return [nid]

----------------------------------------------------------------------------
-- 
---------------------------------------------------------------------------- 

class Reify a where 
    reify :: a -> Capture [NodeID]

runR r = evalStateT r capState 
    where 
      capState = 
         CaptState IntMap.empty 
                   Map.empty
                   0
                   Map.empty


---------------------------------------------------------------------------- 
-- 
instance Reify Expr where 
    reify e@(Var v) = insertNode e (NVar v) 
    reify e@(Lit l) = insertNode e (NLit l)
    reify e@(Index0 exp) = 
        do
          [exp'] <- reify exp 
          insertNode e (NIndex0 exp')
    reify e@(ResIndex exp i) = 
        do 
          [exp'] <- reify exp 
          insertNode e (NResIndex exp' i) 

    -- TODO: CALL and MAP needs to change a lot (future work) 
    reify e@(Call cap exprs) = 
        do 
          fid <- lift (runR cap) 
          exprs' <- mapM reify exprs 
          insertNode e (NCall fid (concat exprs'))  --Concat... make sure this is as it shall
    reify e@(Map cap exprs) = 
        do
          -- Here I need to get something from the backend .. (a function identifier) 
          fid  <- lift ( runR cap )
          liftIO$ putStrLn $ "generated map fun has id: " ++ show fid
          exprs' <- mapM reify exprs 
          insertNode e (NMap fid (concat exprs'))
                    
    reify e@(If e1 e2 e3) = 
        do
          [e1'] <- reify e1
          [e2'] <- reify e2
          [e3'] <- reify e3
          insertNode e (NIf e1' e2' e3')
    reify e@(Op op exprs) = 
        do
          exprs' <- mapM reify exprs 
          insertNode e (NOp op (concat exprs')) 
         
instance Reify (Exp a) where 
    reify = reify . unE 

instance ReifyableFun a b => Reify (a -> b) where 
    reify f = 
        do
          exprs <- reifyFun f
          nids <- mapM reify exprs  
          return $ concat nids 

----------------------------------------------------------------------------
-- 
class ReifyableFun a b where 
    reifyFun :: (a -> b) -> Capture [Expr]


instance Data a => ReifyableFun (Exp a) (Exp b) where 
    reifyFun f = 
        do 
          v <- newVar 
          let t = typeOf (undefined :: a) 
          addVarType v t 
          let e = f $ E (Var v)  
                     
          return [unE e]

instance Data a => ReifyableFun (Exp a) (Exp b,Exp c) where 
    reifyFun f = 
        do 
          v <- newVar 
          let t = typeOf (undefined :: a) 
          addVarType v t 
          let (e1,e2) = f $ E (Var v)  
                     
          return [unE e1,unE e2]

instance Data a => ReifyableFun (Exp a) (Exp b,Exp c, Exp d) where 
    reifyFun f = 
        do 
          v <- newVar 
          let t = typeOf (undefined :: a) 
          addVarType v t 
          let (e1,e2,e3) = f $ E (Var v)  
                     
          return [unE e1,unE e2,unE e3]


instance (Data a, ReifyableFun b c ) => ReifyableFun (Exp a)  (b -> c) where 
    reifyFun f = 
        do 
          v <- newVar 
          let t = typeOf (undefined :: a)
          addVarType v t 
          reifyFun $ f (E (Var v))
