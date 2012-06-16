{-# Language TypeOperators, 
             ScopedTypeVariables, 
             GeneralizedNewtypeDeriving,
             FlexibleContexts,
             FlexibleInstances,
             UndecidableInstances,
             OverlappingInstances,
             CPP #-} 

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
import           Intel.ArBB.Data.Int 
import           Intel.ArBB.Vector
import           Intel.ArBB.IsScalar
import           Intel.ArBB.ReifyableType
import           Intel.ArBB.GenRecord
import           Intel.ArBB.MonadCapture
import           Intel.ArBB.Function

import           Intel.ArBB.Backend.ArBB.CodeGen
-- import           Intel.ArBB.Backend.ArBB.ArBBVector
import           Intel.ArBB.Backend.Vector
import           Intel.ArBB.Backend.Scalar

import qualified Data.Vector.Storable as V 
import qualified Data.Vector.Storable.Mutable as M
import Foreign.ForeignPtr 
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import System.Mem.StableName

import qualified Control.Monad.State.Strict as S
import qualified Data.Map as Map
import qualified Data.Traversable as Trav

import Data.Word
import Data.Int

----------------------------------------------------------------------------
-- the backend.. 
newtype ArBBBackend a = ArBBBackend {unArBBBackend :: (S.StateT ArBBState VM.EmitArbb a)}
    deriving (Monad, S.MonadState ArBBState, S.MonadIO, Functor) 

-- String FunctionName or FunctionID
data ArBBState = ArBBState { arbbFunMap :: Map.Map Integer (VM.ConvFunction, [Type], [Type])
                           , arbbVarMap :: Map.Map Integer VM.Variable -- backend IDs  to ArBB vector ma
                           , arbbUnique :: Integer } 
--TODO: needs to deal with Scalars in pretty much the same way as dvectors.
 
--      A type that refers to a scalar value on the ArBB side is needed. 
--      It should hold just an identifier pointing out a Variable.

type ArBBVector = BEDVector 

---------------------------------------------------------------------------- 
-- 
liftVM :: VM.EmitArbb a -> ArBBBackend a 
liftVM a = ArBBBackend (S.lift a) 

liftIO :: IO a -> ArBBBackend a 
liftIO a = ArBBBackend (S.liftIO a)

-- corresponds to the withArBB monad
runArBBBackend :: ArBBBackend a -> IO a 
runArBBBackend a = VM.arbbSession$ S.evalStateT (unArBBBackend a) arbbState
    where 
      arbbState = ArBBState Map.empty
                            Map.empty
                            0


runArBBBackendAll a = VM.arbbSession$ S.runStateT (unArBBBackend a) arbbState
    where 
      arbbState = ArBBState Map.empty
                            Map.empty
                            0


---------------------------------------------------------------------------- 
-- ArBB MONAD STUFF
----------------------------------------------------------------------------

type ArBB a = ArBBBackend a  

withArBB a = runArBBBackend a


----------------------------------------------------------------------------
-- Backend functions
getFunID :: ArBBBackend  FuncID  
getFunID = 
    do 
      uniq <- S.gets arbbUnique 
      S.modify $ \s -> s {arbbUnique = uniq + 1} 
      return uniq 

getFunMap :: ArBBBackend (Map.Map Integer (VM.ConvFunction, [Type], [Type]))
getFunMap = S.gets arbbFunMap 

addFunction :: FuncID -> VM.ConvFunction -> [Type] -> [Type] -> ArBBBackend ()
addFunction fid fd ins outs = 
    do
      m <- getFunMap 
      S.modify $ \s -> s { arbbFunMap = Map.insert fid (fd,ins,outs) m } 



captureGenRecord :: GenRecord -> ArBBBackend FuncID 
captureGenRecord gr = 
    do 

      --------------------------------------------------
      -- Begin with capturing any functions that this one 
      -- calls or maps      
      depsMap <- Trav.mapM captureGenRecord (genRecordDepends gr)
      --------------------------------------------------
      
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
              vs <- accmBody d nids vt funMap depsMap (zip names is) 
              copyAll os vs

      fid <- getFunID 

      addFunction fid fd tins touts
      
      -- liftIO$ putStrLn $ "captured: " ++ show fid 
 
      return fid 
  
--            (FlexibleContext)
capture :: Reify (a -> b) => (a -> b) -> ArBB (Function (FunIn a b) (FunOut b))
capture d = 
    do 
      d' <- liftIO (runR (reify d))
      fid <- captureGenRecord d'
      return $ Function fid 

serialize :: Function i o  -> ArBBBackend String 
serialize (Function fid) = 
    do 
      m <- getFunMap 
      -- let fid' = "f" ++ show fid 
      case Map.lookup fid m of 
        Nothing -> error "serialize: invalid function"
        (Just (f,tins,touts)) ->
            do 
              str <- liftVM $ VM.serializeFunction_ f
              return (VM.getCString str) 
  

execute :: (VariableList a, VariableList b) => Function a b -> a -> b -> ArBB ()       
execute (Function fid) a b = 
  do 
    (ArBBState mf mv _) <- S.get 
    case Map.lookup fid  mf of 
      Nothing -> error "execute: Invalid function" 
      (Just (f,tins,touts)) -> 
        do 
          ins <- vlist a 
          outs <- vlist b
          
          liftVM$ VM.execute_ f outs ins 
         
          return ()
instance VariableList (BEDVector t a) where 
    vlist v = 
        do 
          (ArBBState  _ mv _) <- S.get 
          case Map.lookup (beDVectorID v) mv of 
            (Just v) -> return [v] 
            Nothing  -> error "ArBB version of vector not found!"
-- TODO: Needs More Work 
class VariableList a where 
    vlist :: a -> ArBB [VM.Variable]

-- TODO: Add the scalar cases 
#define ScalarVList(t,load)                      \
  instance VariableList (t) where {              \
     vlist a = S.liftM (:[]) $ liftVM $ VM.load  a}
                   
--instance VariableList (ArBBRef a) where 
--    vlist (ArBBRef v) = return [v]

instance VariableList (BEScalar a) where 
  vlist (BEScalar i) = undefined
      -- case Map.Lookup

--instance (Ref a) => VariableList a where 
--    vlist a = 
--        do 
--          (ArBBRef v) <- mkRef a 
--          return [v]
         
instance VariableList Int where 
    vlist a = 
        case sizeOf a of 
          4 -> S.liftM (:[]) $ liftVM $ VM.int32_ a
          8 -> S.liftM (:[]) $ liftVM $ VM.int64_ a
          _ -> error "Platform not supported"

instance VariableList  Word where 
    vlist a = 
        case sizeOf  a of 
          4 -> S.liftM (:[]) $ liftVM $ VM.uint32_  a
          8 -> S.liftM (:[]) $ liftVM $ VM.uint64_  a
          _ -> error "Platform not supported" 
               

-- ScalarVList(Int,int64_) -- incorrect on 32bit archs
ScalarVList(Int8,int8_)
ScalarVList(Int16,int16_)
ScalarVList(Int32,int32_)
ScalarVList(Int64,int64_)
-- ScalarVList(Word,uint64_)
ScalarVList(Word8,uint8_)
ScalarVList(Word16,uint16_)
ScalarVList(Word32,uint32_)
ScalarVList(Word64,uint64_)
ScalarVList(Float,float32_)
ScalarVList(Double,float64_)



instance (VariableList t, VariableList rest) => VariableList (t :- rest) where 
    vlist (v :- r) = 
        do
          v' <- vlist v 
          r' <- vlist r 
          return (v' ++ r')



----------------------------------------------------------------------------
-- Copy data into ArBB 
copyIn :: (Data a, IsScalar a, V.Storable a, Dimensions t) => V.Vector a -> t -> ArBB (BEDVector t a)
copyIn dat t = 
  do 
   -- TODO: Bad. Looking at elements of Dat 
   let elem = dat V.! 0 
   [st] <- liftVM$ toArBBType (scalarType elem)             
   dt <- liftVM$ VM.getDenseType_ st ndims 
   
   let (fptr,n') = V.unsafeToForeignPtr0 dat
       ptr       = unsafeForeignPtrToPtr fptr

   g <- liftVM$ VM.createGlobal_nobind_ dt "output"
   v <- liftVM$ VM.variableFromGlobal_ g  
 
   ss <- liftVM$ mapM VM.usize_ dims'
   liftVM$ VM.opDynamicImm_ VM.ArbbOpAlloc [v] ss


   -- get a ptr to the data in the VM and copy Haskell vector into it. 
   arbbdat <- liftVM$ VM.mapToHost_ v (map fromIntegral dims') VM.ArbbWriteOnlyRange
   liftIO$  copyBytes (castPtr arbbdat) 
                      ptr
                      ((foldl (*) 1 dims') * sizeOf elem) 

   (ArBBState mf mv i) <- S.get
   
   -- TODO: an addVector function.. 
   let mv' = Map.insert i v mv
                
   S.put (ArBBState mf mv' (i+1))

   return $ BEDVector i dims
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
new :: (IsScalar a, V.Storable a, Dimensions t) => t -> a -> ArBB (BEDVector t a)
new t a = 
  do
   [st] <- liftVM$ toArBBType (scalarType a)             
   dt <- liftVM$ VM.getDenseType_ st ndims   
   
   g <- liftVM$ VM.createGlobal_nobind_ dt "output"
   v <- liftVM$ VM.variableFromGlobal_ g  
 
   ss <- liftVM$ mapM VM.usize_ (dimList dims)
   liftVM$ VM.opDynamicImm_ VM.ArbbOpAlloc [v] ss

   -- TODO : Fill the vector!
    

   (ArBBState mf mv i) <- S.get 
   
   let mv' = Map.insert i v mv 
                
   S.put (ArBBState mf mv' (i+1))

   return $ BEDVector i dims
  where 
     -- TODO: There should be some insurance that ndims is 1,2 or 3.
     --       Or a way to handle the higher dimensionalities. 
     ndims = dimensions dims 
     dims = toDim t    
    



----------------------------------------------------------------------------
-- Copy data out of ArBB 
copyOut :: (Data a, IsScalar a, V.Storable a, Dimensions t) 
         =>  BEDVector t a  -> ArBB (V.Vector a) 
copyOut dv = 
  do 
   (vec :: M.IOVector a)  <- liftIO$ M.new $ (foldl (*) 1 dims')

   let (fptr,n') = M.unsafeToForeignPtr0 vec
       ptr       = unsafeForeignPtrToPtr fptr


   (ArBBState  _ mv _) <- S.get                    
  
   let (Just v) = Map.lookup (beDVectorID dv) mv
   arbbdat <- liftVM$ VM.mapToHost_ v (map fromIntegral dims') VM.ArbbReadOnlyRange
   liftIO$  copyBytes ptr
                      (castPtr arbbdat) 
                      ((foldl (*) 1 dims') * (sizeOf (undefined :: a)) ) 
                      
   liftIO$ V.freeze vec

   -- return out
   
   where 
     -- TODO: There should be some insurance that ndims is 1,2 or 3.
     --       Or a way to handle the higher dimensionalities. 
     ndims = length dims'
     dims = beDVectorShape dv
     (Dim dims') = dims -- TODO: FIX FIX 

--TODO: Someway to copy a scalar out.. (readScalar) 

--readScalar :: (Data a , IsScalar a) => 
--              a -> ArBB a 



----------------------------------------------------------------------------
-- TODO: require an ID system of the backen. this can make the typefamily business 
--       below easier (a one time thing) 
-- 

----------------------------------------------------------------------------
-- References to scalars that live on the ArBB side.
{-  
data ArBBRef a = ArBBRef {arbbRefVar :: VM.Variable}

class Ref a where
    mkRef :: a -> ArBB (ArBBRef a)


#define RefScal(ty,load)                   \
  instance Ref (ty) where  {                  \
    mkRef a =                              \
      do {                                  \
          v <- liftVM $ VM.load  a;           \
          return $ ArBBRef v }} 

RefScal(Int,int64_) -- fix for 32/64 bit
RefScal(Int8,int8_)
RefScal(Int16,int16_)
RefScal(Int32,int32_)
RefScal(Int64,int64_)
RefScal(Word,uint64_)
RefScal(Word8,uint8_)
RefScal(Word16,uint16_)
RefScal(Word32,uint32_)
RefScal(Word64,uint64_)
RefScal(Float,float32_)
RefScal(Double,float64_)
----------------------------------------------------------------------------
-- Creating the input output baviour of the ArBB backend.
-- TODO: Talk to some expert about this.. (Emil, Koen, ? ) 
-} 

