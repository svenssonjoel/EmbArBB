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
import           Intel.ArBB.GenArBB
 
-- BackendExperiment, towards removing the step via LExp in the codegeneration
-- TODO: implement for a fixed  ArBB Backend. 
-- TODO: generalise to taking the backend as a parameter somehow. 

----------------------------------------------------------------------------
-- the backend.. 
newtype ArBBBackend a = ArBBBackend {unArBBBackend :: (StateT ArBBState VM.EmitArbb a)}
    deriving (Monad, MonadState ArBBState, MonadIO, Functor) 

type ArBBState = ( Map.Map FunctionName (VM.ConvFunction, [Type], [Type])
                 , Map.Map Integer VM.Variable -- dVectorID to ArBB vector map
                 , Integer)

liftVM :: VM.EmitArbb a -> ArBBBackend a 
liftVM a = ArBBBackend (lift a) 

-- Sketch of new withArBB
runArBBBackend :: ArBBBackend a -> IO a 
runArBBBackend a = VM.arbbSession$ evalStateT (unArBBBackend a) arbbState
    where 
      arbbState = (Map.empty
                  ,Map.empty
                  ,0)


runArBBBackendAll a = VM.arbbSession$ runStateT (unArBBBackend a) arbbState
    where 
      arbbState = (Map.empty
                  ,Map.empty
                  ,0)


--withArBB :: Capture ArBBBackend a -> IO a 
--withArBB 



class (Monad m, MonadIO m ) => MonadBackend m 

instance MonadBackend ArBBBackend 


---------------------------------------------------------------------------- 
-- A new attempt at ArBB monad. 
type ArBB a = Capture ArBBBackend a 

withArBB a = runArBBBackend (runR a)



getFunID :: ArBB FuncID  
getFunID = 
    do 
      funId <- (lift . gets) (\(_,_,x) -> x)
      lift . modify $ \(a,b,c) -> (a,b,c+1)
      return funId 

getFunMap :: ArBB (Map.Map FunctionName (VM.ConvFunction, [Type], [Type]))
getFunMap = (lift . gets) (\(m,_,_) -> m) 

addFunction :: FuncID -> VM.ConvFunction -> [Type] -> [Type] -> ArBB ()
addFunction fid fd ins outs = 
    do
      m <- getFunMap 
      let fid' = "f" ++ show fid 
      lift . modify $ \(m,b,c) -> (Map.insert fid' (fd,ins,outs) m,b,c)
----------------------------------------------------------------------------
-- functions that are not backend oblivious 
-- TODO: Something else then FuncID should be returned. Something typed. 
capture :: ReifyableFun a b => (a -> b) -> ArBB FuncID 
capture f = 
    do 
      fid <- getFunID 
      nids <- reify f 


      -- Cheat (with the types)
      arbbIns  <-  lift . liftVM $ mapM toArBBType tins 
      arbbOuts <-  lift . liftVM $ mapM toArBBType touts 
      let names = [Variable ("v"++show i) | i <- [0..]]
                   
      funMap <- getFunMap
      
      d <- gets dag 
      vt <- gets types
    
      fd <- lift . liftVM $ VM.funDef_ "generated" (concat arbbOuts) (concat arbbIns) $ \ os is -> 
            do 
              vs <- accmBody d nids vt funMap (zip names is) 
              copyAll os vs

      addFunction fid fd tins touts
 
      return fid 
          where 
            tins = [Scalar VM.ArbbU32, Dense I VM.ArbbU32]
            touts = [Dense I VM.ArbbU32]
serialize :: FuncID -> ArBB String 
serialize fid = 
    do 
      m <- getFunMap 
      let fid' = "f" ++ show fid 
      case Map.lookup fid' m of 
        Nothing -> error "serialize: invalid function"
        (Just (f,tins,touts)) ->
            do 
              str <- lift . liftVM $ VM.serializeFunction_ f
              return (VM.getCString str) 

{- 
--liftIO$ putStrLn "cap1"
   
      

    
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
-} 


----------------------------------------------------------------------------

type FuncID = Integer 
type VarID  = Integer 

data BEFunction i o = BEFunction FuncID  --similar to old function in Syntax.hs.

----------------------------------------------------------------------------
data CaptState = CaptState { sharing :: IntMap.IntMap [(Dynamic,Integer)]
                           , types   :: VarType
                           , unique  :: Integer 
                           , dag     :: DAG }  
               deriving Show

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
               
---------------------------------------------------------------------------- 
--class Capturable a where 
--    capture ::  a -> Capture ArBBBackend FuncID 

--instance ReifyableFun a b =>  Capturable (a -> b) where 
--    capture f = undefined 
---------------------------------------------------------------------------- 

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
-- Steal most ArBB code generation from previous version (For now) 


