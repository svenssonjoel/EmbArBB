{-# LANGUAGE GeneralizedNewtypeDeriving, 
             MultiParamTypeClasses, 
             FlexibleInstances, 
             ScopedTypeVariables #-}

module Intel.ArBB.BackendExperiment where 

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
newtype ArBBBackend a = ArBBBackend (StateT ArBBState IO a)
    deriving (Monad, MonadState ArBBState, MonadIO, Functor) 

type ArBBState = ( Map.Map FunctionName (VM.ConvFunction, [Type], [Type])
                 , Map.Map Integer VM.Variable -- dVectorID to ArBB vector map
                 , Integer)

class (Monad m, MonadIO m ) => MonadBackend m

----------------------------------------------------------------------------

type FuncID = Int 
type VarID  = Int 

----------------------------------------------------------------------------
data CaptState = CaptState { sharing :: IntMap.IntMap [(Dynamic,Integer)]
                           , types   :: VarType
                           , unique  :: Integer 
                           , dag     :: DAG }  

type Capture backend a = StateT CaptState backend a 

newVar :: MonadBackend backend => Capture backend Variable 
newVar = do 
  id <- gets unique 
  modify $ \s -> s { unique = id + 1 } 
  return $ Variable ("v" ++ show id)

addVarType :: MonadBackend backend => Variable -> Type -> Capture backend () 
addVarType v t = do 
  m <- gets types 
  modify $ \s -> s { types = Map.insert v t m } 

-- TODO: DO I need [(Dynamic,Expr)], 
--- maybe for my needs something like [(StableName Expr,Integer)] is better .. 
-- TODO: Do not use the hash as NODEID !!! 
-- TOOD: Break up into tiny functions. 
addToDAG :: MonadBackend backend => Expr -> Capture backend NodeID
addToDAG e = 
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


                
    

      

---------------------------------------------------------------------------- 
class Capturable a where 
    capture ::  a -> Capture ArBBBackend FuncID 

---------------------------------------------------------------------------- 

-- will this be a working approach ?
-- backend wont matter while creating the DAG.. 
-- I am probably approaching this backwards though. 
class Reify a where 
    reify :: MonadBackend backend => a -> Capture backend ([NodeID],DAG) 
    
instance Reify Expr where 
    reify expr = undefined 

instance Reify (Exp a) where 
    reify = reify . unE 


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


        
       



----------------------------------------------------------------------------
-- Steal most ArBB code generation from previous version (For now) 


