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
import qualified Data.Vector.Storable as V 
import qualified Data.Vector.Storable.Mutable as M
import Foreign.ForeignPtr 
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import System.Mem.StableName

import qualified Intel.ArbbVM as VM
import qualified Intel.ArbbVM.Convenience as VM hiding (liftIO) 
import           Intel.ArBB.DAG
import           Intel.ArBB.TypeCheck
import           Intel.ArBB.Types 
import           Intel.ArBB.Syntax
import           Intel.ArBB.Data
import           Intel.ArBB.GenArBB
import           Intel.ArBB.Vector
import           Intel.ArBB.ReifyableType
import           Intel.ArBB.IsScalar
 
import Data.Int
import Data.Word
import Intel.ArBB.Data.Int

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
-- ArBB MONAD STUFF
----------------------------------------------------------------------------

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
capture :: (ReifyableFunType a b, ReifyableFun a b) => (a -> b) -> ArBB FuncID 
capture f = 
    do 
      fid <- getFunID 
      nids <- reify f 
      
      let funType = reifyFunType f 
          tins  = getInputTypes funType
          touts = case getOutputType funType of 
                    (Tuple xs) -> xs 
                    a          -> [a]
      -- TODO: reifyFunType may potentially return a much more 
      --       complicated type than the ArBB backend supports.
      --       Right now I dont think that kind of functions have 
      --       been tried. Once they are, some kind of compilation 
      --       into simpler typed objects is needed... 
              
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

-- A VERY Cheaty version of Execute 
execute :: (VariableList a, VariableList b) => FuncID {-Function a b-} -> a -> b -> ArBB ()       
execute fid {-(Function fn)-} a b = 
  do 
    (mf,mv,_) <- lift get 
    case Map.lookup ("f" ++ show fid) {-fn-} mf of 
      Nothing -> error "execute: Invalid function" 
      (Just (f,tins,touts)) -> 
        do 
          ins <- vlist a 
          outs <- vlist b
          
          lift . liftVM$ VM.execute_ f outs ins 
         
          return ()

class VariableList a where 
    vlist :: a -> ArBB [VM.Variable]

-- TODO: Add the scalar cases

instance VariableList (DVector t a) where 
    vlist v = 
        do 
          (_,mv,_) <- lift get 
          case Map.lookup (dVectorID v) mv of 
            (Just v) -> return [v] 
            Nothing  -> error "ArBB version of vector not found!"
instance (VariableList t, VariableList rest) => VariableList (t :- rest) where 
    vlist (v :- r) = 
        do
          v' <- vlist v 
          r' <- vlist r 
          return (v' ++ r')



----------------------------------------------------------------------------
-- Copy data into ArBB 
copyIn :: (Data a, IsScalar a, V.Storable a, Dimensions t) => V.Vector a -> t -> ArBB (DVector t a) 
copyIn dat t = 
  do 
   -- TODO: Bad. Looking at elements of Dat 
   let elem = dat V.! 0 
   [st] <- lift . liftVM$ toArBBType (scalarType elem)             
   dt <- lift . liftVM$ VM.getDenseType_ st ndims 
   
   let (fptr,n') = V.unsafeToForeignPtr0 dat
       ptr       = unsafeForeignPtrToPtr fptr

   g <- lift . liftVM$ VM.createGlobal_nobind_ dt "output"
   v <- lift . liftVM$ VM.variableFromGlobal_ g  
 
   ss <- lift . liftVM$ mapM VM.usize_ dims'
   lift . liftVM$ VM.opDynamicImm_ VM.ArbbOpAlloc [v] ss

   -- TODO: copy the data! 

   arbbdat <- lift . liftVM$ VM.mapToHost_ v (map fromIntegral dims') VM.ArbbWriteOnlyRange
   liftIO$  copyBytes (castPtr arbbdat) 
                      ptr
                      ((foldl (*) 1 dims') * sizeOf elem) 

   (mf,mv,i) <- lift get
   
   let mv' = Map.insert i v mv 
                
   (lift . put) (mf,mv',i+1)

   return (DVector i dims)
   -- TODO: Figure out if this is one of these cases where makeRefCountable is needed..
   -- TODO: figure out if it is possible to let Haskell Garbage collector 
   --       "free" arbb-allocated memory. 
   where 
     -- TODO: There should be some insurance that ndims is 1,2 or 3.
     --       Or a way to handle the higher dimensionalities. 
     ndims = length dims'
     dims = toDim t    
     (Dim dims') = dims -- TODO: FIX FIX 

-- create a new DVector with same element at all indices. 
-- TODO: Actually fill it with the elements (constVector)
new :: (IsScalar a, V.Storable a, Dimensions t) => t -> a -> ArBB (DVector t a)
new t a = 
  do
   [st] <- lift . liftVM$ toArBBType (scalarType a)             
   dt <- lift . liftVM$ VM.getDenseType_ st ndims   
   
   g <- lift . liftVM$ VM.createGlobal_nobind_ dt "output"
   v <- lift .liftVM$ VM.variableFromGlobal_ g  
 
   ss <- lift . liftVM$ mapM VM.usize_ (dimList dims)
   lift . liftVM$ VM.opDynamicImm_ VM.ArbbOpAlloc [v] ss

   -- TODO : Fill the vector!
    

   (mf,mv,i) <- lift get 
   
   let mv' = Map.insert i v mv 
                
   (lift . put) (mf,mv',i+1)

   return (DVector i dims)
  where 
     -- TODO: There should be some insurance that ndims is 1,2 or 3.
     --       Or a way to handle the higher dimensionalities. 
     ndims = dimensions dims 
     dims = toDim t    
    



----------------------------------------------------------------------------
-- Copy data out of ArBB 
copyOut :: (Data a, IsScalar a, V.Storable a, Dimensions t) 
         => DVector t a -> ArBB (V.Vector a) 
copyOut dv = 
  do 
   (vec :: M.IOVector a)  <- liftIO$ M.new $ (foldl (*) 1 dims')

   let (fptr,n') = M.unsafeToForeignPtr0 vec
       ptr       = unsafeForeignPtrToPtr fptr


   (_,mv,_) <- lift get                    
   -- TODO: STOP CHEATING! 
   let (Just v) = Map.lookup (dVectorID dv) mv
   arbbdat <- lift . liftVM$ VM.mapToHost_ v (map fromIntegral dims') VM.ArbbReadOnlyRange
   liftIO$  copyBytes ptr
                      (castPtr arbbdat) 
                      ((foldl (*) 1 dims') * 4) -- CHEAT  
                      
   liftIO$ V.freeze vec

   -- return out
   
   where 
     -- TODO: There should be some insurance that ndims is 1,2 or 3.
     --       Or a way to handle the higher dimensionalities. 
     ndims = length dims'
     dims = dVectorShape dv
     (Dim dims') = dims -- TODO: FIX FIX 

----------------------------------------------------------------------------
-- The Capture Monad Stuff 
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



