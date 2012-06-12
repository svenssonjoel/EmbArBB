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
import Intel.ArBB.MonadBackend
import Intel.ArBB.MonadCapture
 
import Data.Int
import Data.Word
import Intel.ArBB.Data.Int

----------------------------------------------------------------------------
-- The Capture Monad Stuff 
----------------------------------------------------------------------------

type FuncID = Integer 
type VarID  = Integer 

-- data BEFunction i o = BEFunction FuncID  --similar to old function in Syntax.hs.

----------------------------------------------------------------------------
{- 
data CaptState = CaptState { sharing :: IntMap.IntMap [(Dynamic,Integer)]
                           , types   :: Map.Map Variable Type -- VarType
                           , unique  :: Integer 
                           , dag     :: DAG }  
               deriving Show

type Capture backend a = StateT CaptState backend a 
-} 

newVar :: MonadBackend backend => Capture backend Variable 
newVar = do 
  id <- gets unique 
  modify $ \s -> s { unique = id + 1 } 
  return $ Variable ("v" ++ show id)

addVarType :: MonadBackend backend => Variable -> Type -> Capture backend () 
addVarType v t = do 
  m <- gets types 
  modify $ \s -> s { types = Map.insert v t m } 



-- TODO: Break up into tiny functions. 
-- TODO: I dont really need the Dynamic right ?
getNodeID :: MonadBackend backend => Expr -> Capture backend NodeID
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


insertNode :: MonadBackend backend => Expr -> Node -> Capture backend [NodeID]
insertNode e node = 
    do 
      d <- gets dag
      nid <- getNodeID e 
      let d' = Map.insert nid node d 
      modify $ \s -> s {dag = d'} 
      return [nid]
               

-- will this be a working approach ?
-- backend wont matter while creating the DAG.. 
-- I am probably approaching this backwards though. 
-- The thing is that I need the backend to obtain a function identifier
-- in the case of call or map. 
class Reify a where 
    reify :: MonadBackend backend => a -> Capture backend [NodeID]

runR r = evalStateT r capState 
    where 
      capState = 
         CaptState IntMap.empty 
                   Map.empty
                   0
                   Map.empty

rR r = runStateT r capState 
    where 
      capState = 
         CaptState IntMap.empty 
                   Map.empty
                   0
                   Map.empty



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
    reify e@(Call fn exprs) = 
        do 
          exprs' <- mapM reify exprs 
          insertNode e (NCall fn (concat exprs'))  --Concat... make sure this is as it shall
    reify e@(Map fn exprs) = 
        do 
          exprs' <- mapM reify exprs 
          insertNode e (NMap fn (concat exprs'))  --Concat... make sure this is as it shall
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
    reifyFun :: MonadBackend backend =>  (a -> b) -> Capture backend [Expr]


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
        
       


----------------------------------------------------------------------------
-- Typed interface for captured functions. 



