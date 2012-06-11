{-# LANGUAGE GeneralizedNewtypeDeriving, 
             MultiParamTypeClasses, 
             FlexibleInstances, 
             ScopedTypeVariables #-}

module Intel.ArBB.BackendExperiment2 where 

import Control.Monad.State.Strict
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.Dynamic as Dynamic hiding (typeOf)  

import System.Mem.StableName

import qualified Intel.ArbbVM as VM
import qualified Intel.ArbbVM.Convenience as VM hiding (liftIO) 
import           Intel.ArBB.DAG
import           Intel.ArBB.TypeCheck
import           Intel.ArBB.Types 
import           Intel.ArBB.Syntax
import           Intel.ArBB.Data
 
-- BackendExperiment, towards removing the step via LExp in the codegeneration
-- TODO: implement for a fixed  ArBB Backend. 
-- TODO: generalise to taking the backend as a parameter somehow. 

----------------------------------------------------------------------------
-- the backend.. 
{- 
newtype ArBBBackend a = ArBBBackend {unArBBBackend :: (StateT ArBBState VM.EmitArbb a)}
    deriving (Monad, MonadState ArBBState, MonadIO, Functor) 

type ArBBState = ( Map.Map FunctionName (VM.ConvFunction, [Type], [Type])
                 , Map.Map Integer VM.Variable -- dVectorID to ArBB vector map
                 , Integer)

-- Sketch of new withArBB
runArBB :: ArBBBackend a -> IO a 
runArBB a = VM.arbbSession$ evalStateT (unArBBBackend a) arbbState
    where 
      arbbState = (Map.empty
                  ,Map.empty
                  ,0)

--withArBB :: Capture ArBBBackend a -> IO a 
--withArBB 



class (Monad m, MonadIO m ) => MonadBackend m 

instance MonadBackend ArBBBackend 
-} 
-- TODO: think more before jumping into these.. 
-- runBackend :: MonadBackend backend => backend a -> a 
-- runBackend b = runState b 

type ArBB = 


----------------------------------------------------------------------------

type FuncID = Int 
type VarID  = Int 

data BEFunction i o = BEFunction FuncID  --similar to old function in Syntax.hs.

----------------------------------------------------------------------------
data RState = RState { sharing :: IntMap.IntMap [(Dynamic,Integer)]
                     , types   :: VarType
                     , unique  :: Integer 
                     , dag     :: DAG }  
            deriving Show 

newtype R a = R {unR :: StateT RState IO a}
    deriving (Monad, MonadIO, MonadState RState)

newVar :: R Variable 
newVar = do 
  id <- gets unique 
  modify $ \s -> s { unique = id + 1 } 
  return $ Variable ("v" ++ show id)

addVarType :: Variable -> Type -> R () 
addVarType v t = do 
  m <- gets types 
  modify $ \s -> s { types = Map.insert v t m } 
 
getNodeID :: Expr -> R NodeID
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


insertNode :: Expr -> Node -> R [NodeID]
insertNode e node = 
    do 
      d <- gets dag
      nid <- getNodeID e 
      let d' = Map.insert nid node d 
      modify $ \s -> s {dag = d'} 
      return [nid]
               
---------------------------------------------------------------------------- 


class Reify a where 
    reify :: a -> R [NodeID]

runR :: R a -> IO a 
runR r = evalStateT (unR r) rState 
    where 
      rState = 
         RState IntMap.empty 
                   Map.empty
                   0
                   Map.empty

rR r = runStateT (unR r) rState 
    where 
      rState = 
         RState IntMap.empty 
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
    reifyFun :: (a -> b) -> R [Expr]


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
-- Steal most ArBB code generation from previous version (For now) 


