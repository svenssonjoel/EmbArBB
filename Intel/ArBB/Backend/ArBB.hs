{-# Language TypeOperators, 
             ScopedTypeVariables, 
             GeneralizedNewtypeDeriving #-} 

module Intel.ArBB.Backend.ArBB where 

--import Intel.ArBB.MonadBackend
--import Intel.ArBB.MonadCapture
import Intel.ArBB.BackendExperiment

import qualified Intel.ArbbVM as VM
import qualified Intel.ArbbVM.Convenience as VM hiding (liftIO) 
import           Intel.ArBB.DAG
import           Intel.ArBB.TypeCheck
import           Intel.ArBB.Types 
import           Intel.ArBB.Syntax
import           Intel.ArBB.Variable
import           Intel.ArBB.Literal
import           Intel.ArBB.Op

import           Intel.ArBB.Data
import           Intel.ArBB.Backend.ArBB.CodeGen
import           Intel.ArBB.Vector
import           Intel.ArBB.IsScalar
import           Intel.ArBB.ReifyableType
import           Intel.ArBB.GenRecord
import           Intel.ArBB.MonadCapture

import qualified Data.Vector.Storable as V 
import qualified Data.Vector.Storable.Mutable as M
import Foreign.ForeignPtr 
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import System.Mem.StableName

import Control.Monad.State.Strict
import qualified Data.Map as Map

type FuncID = Integer

-- BackendExperiment, towards removing the step via LExp in the codegeneration
-- TODO: implement for a fixed  ArBB Backend. 
-- TODO: generalise to taking the backend as a parameter somehow. 

----------------------------------------------------------------------------
-- the backend.. 
--newtype ArBBBackend a = ArBBBackend {unArBBBackend :: (StateT ArBBState VM.EmitArbb a)}
--    deriving (Monad, MonadState ArBBState, MonadIO, Functor) 

-- String FunctionName or FunctionID
--type ArBBState = ( Map.Map String (VM.ConvFunction, [Type], [Type])
--                 , Map.Map Integer VM.Variable -- dVectorID to ArBB vector map
--                 , Integer)

--instance MonadBackend ArBBBackend 
   -- currently empty ! 


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




---------------------------------------------------------------------------- 
-- ArBB MONAD STUFF
----------------------------------------------------------------------------

type ArBB a = Capture a 

withArBB a = runArBBBackend (runR a)


----------------------------------------------------------------------------
-- Backend functions
getFunID :: ArBBBackend  FuncID  
getFunID = 
    do 
      funId <- gets (\(_,_,x) -> x)
      modify $ \(a,b,c) -> (a,b,c+1)
      return funId 

getFunMap :: ArBBBackend (Map.Map String (VM.ConvFunction, [Type], [Type]))
getFunMap = gets (\(m,_,_) -> m) 

addFunction :: FuncID -> VM.ConvFunction -> [Type] -> [Type] -> ArBBBackend ()
addFunction fid fd ins outs = 
    do
      m <- getFunMap 
      let fid' = "f" ++ show fid 
      modify $ \(m,b,c) -> (Map.insert fid' (fd,ins,outs) m,b,c)



captureGenRecord :: GenRecord -> ArBBBackend FuncID 
captureGenRecord gr = 
    do 
      let tins = getInputTypes (genRecordFunType gr)
          touts = case getOutputType (genRecordFunType gr) of 
                    (Tuple xs) -> xs 
                    a          -> [a]
      arbbIns  <- liftVM $ mapM toArBBType tins 
      arbbOuts <- liftVM $ mapM toArBBType touts
      let names = [Variable ("v" ++ show i) | i <- [0..]] 

      funMap <- getFunMap
      
      let d = genRecordDag gr
          vt = genRecordVarType gr 
          nids = genRecordNids gr

      fd <- liftVM $ VM.funDef_ "generated" (concat arbbOuts) (concat arbbIns) $ \ os is -> 
            do 
              vs <- accmBody d nids vt funMap (zip names is) 
              copyAll os vs

      fid <- getFunID 

      addFunction fid fd tins touts
 
      return fid 
                                                 
----------------------------------------------------------------------------
-- functions that are not backend oblivious 
-- TODO: Something else then FuncID should be returned. Something typed. 
capture :: (ReifyableFunType a b, ReifyableFun a b) => (a -> b) -> ArBB FuncID 
capture f = 
    do 
      nids <- reify f 
      
      let funType = reifyFunType f 
      
      d <- gets dag 
      vt <- gets types
      
      let gr = GenRecord d funType nids vt
      
      lift $ captureGenRecord gr
      
{- 
capture :: (ReifyableFunType a b, ReifyableFun a b) => (a -> b) -> ArBB FuncID 
capture f = 
    do 
      fid <- lift getFunID 
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
                   
      funMap <- lift getFunMap
      
      d <- gets dag 
      vt <- gets types
    
      fd <- lift . liftVM $ VM.funDef_ "generated" (concat arbbOuts) (concat arbbIns) $ \ os is -> 
            do 
              vs <- accmBody d nids vt funMap (zip names is) 
              copyAll os vs

      lift $ addFunction fid fd tins touts
 
      return fid 
         
-} 
serialize :: FuncID -> ArBB String 
serialize fid = 
    do 
      m <- lift getFunMap 
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