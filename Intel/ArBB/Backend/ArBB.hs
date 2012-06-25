{-# Language TypeOperators, 
             ScopedTypeVariables, 
             GeneralizedNewtypeDeriving,
             FlexibleContexts,
             FlexibleInstances,
             UndecidableInstances,
             OverlappingInstances,
             CPP #-} 

module Intel.ArBB.Backend.ArBB where 

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
import           Intel.ArBB.MonadReify
import           Intel.ArBB.Reify
import           Intel.ArBB.Function

import           Intel.ArBB.Language hiding (length,map)

import           Intel.ArBB.Backend.ArBB.CodeGen
import           Intel.ArBB.Backend.Vector
import           Intel.ArBB.Backend.Scalar 

import qualified Data.Vector.Storable as V 
import qualified Data.Vector.Storable.Mutable as M
import Foreign.ForeignPtr 
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils hiding (new)
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
                           , arbbVarMap :: Map.Map Integer VM.Variable -- backend IDs  to ArBB vector map
                           , arbbUnique :: Integer } 

--DONE: needs to deal with Scalars in pretty much the same way as dvectors.
 
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

getUnique :: ArBBBackend  Integer  
getUnique = 
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

addVariable :: VM.Variable -> ArBBBackend Integer 
addVariable v = 
    do 
      u <- getUnique
      m <- S.gets arbbVarMap 
      S.modify $ \s -> s {arbbVarMap = Map.insert u v m }
      return u

captureGenRecord :: GenRecord -> ArBBBackend FuncID 
captureGenRecord gr = 
    do 

      --------------------------------------------------
      -- Begin with capturing any functions that this one 
      -- calls or maps  (depends on)     
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
  
--      (a FlexibleContext)
capture :: Reify (a -> b) => (a -> b) -> ArBB (Function (FunIn a b) (FunOut b))
capture d = 
    do 
      d' <- liftIO (runR (reify d))
      fid <- captureGenRecord d'
      
      let h =  genRecordHash d' 
      liftIO$ putStrLn $ "genRecordHash: " ++ show h
      return $ Function fid 

serialize :: Function i o -> ArBBBackend String 
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



----------------------------------------------------------------------------
-- Turn heterogeneous list of 'stuff' into a list of variables. 
class VariableList a where 
    vlist :: a -> ArBB [VM.Variable]



instance VariableList (BEScalar a) where 
  vlist (BEScalar i) = 
      do 
        vm <- S.gets arbbVarMap 
        case Map.lookup i vm of 
          Nothing -> error "vList: Scalar does not excist" 
          (Just v) -> return [v]


instance VariableList (BEDVector t a) where 
    vlist v = 
        do 
          (ArBBState  _ mv _) <- S.get 
          case Map.lookup (beDVectorID v) mv of 
            (Just v) -> return [v] 
            Nothing  -> error "ArBB version of vector not found!"

instance VariableList (BENVector a) where 
    vlist v = 
        do 
          (ArBBState  _ mv _) <- S.get 
          case Map.lookup (beNVectorID v) mv of 
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
 

   -- liftIO $ putStrLn $ "dims: " ++ show dims'
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
   --       "free" arbb-allocated memory. (using refcountables and foreign ptrs) 
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

   -- DONE : Fill the vector!
   arbbdat <- liftVM$ VM.mapToHost_ v (map fromIntegral (dimList dims)) VM.ArbbWriteOnlyRange 
   liftIO$ pokeArray (castPtr arbbdat) (replicate (foldl (*) 1 (dimList dims)) a)


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

   where 
     -- TODO: There should be some insurance that ndims is 1,2 or 3.
     --       Or a way to handle the higher dimensionalities. 
     ndims = length dims'
     dims = beDVectorShape dv
     (Dim dims') = dims -- TODO: FIX FIX 

{-
----------------------------------------------------------------------------
-- NESTED VERSIONS 
----------------------------------------------------------------------------
-- Copy data into ArBB 
{- 
copyInNested :: (Data a, IsScalar a, V.Storable a) 
              => V.Vector a -> V.Vector USize -> ArBB (BENVector a)
copyInNested dat nesting = 
  do 
   -- TODO: Bad. Looking at elements of Dat 
   let elem = dat V.! 0 
   [st1] <- liftVM$ toArBBType (scalarType elem)             
   dt1 <- liftVM$ VM.getDenseType_ st1 1 
         
   [st2] <- liftVM$ toArBBType (Scalar VM.ArbbUsize) 
   dt2 <- liftVM$ VM.getDenseType_ st2 1

   nt1 <- liftVM$ VM.getNestedType_ st1 
   
   g1 <- liftVM$ VM.createGlobal_nobind_ dt1 "i1"
   v1 <- liftVM$ VM.variableFromGlobal_ g1  
 
   g2 <- liftVM$ VM.createGlobal_nobind_ dt2 "i2"
   v2 <- liftVM$ VM.variableFromGlobal_ g2 

   g3 <- liftVM$ VM.createGlobal_nobind_ nt1 "nested"
   v3 <- liftVM$ VM.variableFromGlobal_ g3

   ds <- liftVM$ VM.usize_ datSize
   liftVM$ VM.opDynamicImm_ VM.ArbbOpAlloc [v1] [ds]
   ns <- liftVM$ VM.usize_ nestSize 
   liftVM$ VM.opDynamicImm_ VM.ArbbOpAlloc [v2] [ns]

   let (fptr,_)  = V.unsafeToForeignPtr0 dat
       ptr       = unsafeForeignPtrToPtr fptr
       (fptr2,_) = V.unsafeToForeignPtr0 nesting 
       ptr2      = unsafeForeignPtrToPtr fptr2

   -- get a ptr to the data in the VM and copy Haskell vector into it. 
   arbbdat  <- liftVM$ VM.mapToHost_ v1 [fromIntegral datSize] VM.ArbbWriteOnlyRange
   arbbnest <- liftVM$ VM.mapToHost_ v2 [fromIntegral nestSize] VM.ArbbWriteOnlyRange
   liftIO$  copyBytes (castPtr arbbdat) 
                      ptr
                      (datSize * sizeOf elem) 
   liftIO$  copyBytes (castPtr arbbnest) 
                      ptr2
                      (nestSize * sizeOf elem) 

   vs_length <- liftVM$ VM.usize_ 1
   liftVM$ VM.opImm_ VM.ArbbOpApplyNesting [v3] [v1,v2,vs_length]

   (ArBBState mf mv i) <- S.get
   
   -- TODO: an addVector function.. 
   let mv' = Map.insert i v3 mv
                
   S.put (ArBBState mf mv' (i+1))

   return $ BENVector i datSize nestSize
       where 
         datSize = V.length dat
         nestSize = V.length nesting
-} 
 
newNested :: (IsScalar a, V.Storable a) 
           => Int -> Int -> a -> ArBB (BENVector a)
newNested elements segments a = 
  do
    
    elts <- new (Z:.elements) a 
    segs <- new (Z:.segments) (0 :: USize) 
            
    mv <- S.gets arbbVarMap
            
    liftIO$ putStrLn "Start"
    let (Just ve) = Map.lookup (beDVectorID elts) mv 
        (Just vs) = Map.lookup (beDVectorID segs) mv 
    
    liftIO$ putStrLn "Create types"
    [st] <- liftVM$ toArBBType (scalarType a)             
    nt <- liftVM$ VM.getNestedType_ st 
   
    liftIO$ putStrLn "Create Global and Variable"
    g <- liftVM$ VM.createGlobal_nobind_ nt "fresh"
    v <- liftVM$ VM.variableFromGlobal_ g  
 
    liftIO$ putStrLn "Allocate memory"
    -- ss <- liftVM$ VM.usize_ (fromIntegral elements)
    -- liftVM$ VM.opDynamicImm_ VM.ArbbOpAlloc [v] [ss]

    liftIO$ putStrLn "Apply nesting"
    vs_length <- liftVM$ VM.usize_ 1
    liftVM$ VM.opImm_ VM.ArbbOpApplyNesting [v] [ve,vs,vs_length] 


    liftIO$ putStrLn "Done"
    (ArBBState mf mv i) <- S.get 
   
    let mv' = Map.insert i v mv 
                
    S.put (ArBBState mf mv' (i+1))

    return $ BENVector i elts segs
    



----------------------------------------------------------------------------
-- Copy data out of ArBB 
copyOutNested :: (Data a, IsScalar a, V.Storable a) 
         =>  BENVector a -> ArBB (V.Vector a, V.Vector USize) 
copyOutNested nv = 
  do 
   (ArBBState  _ mv _) <- S.get                    
  
   let (Just v) = Map.lookup (beNVectorID nv) mv
       (Just nd) = Map.lookup (beDVectorID (beNVectorData nv)) mv 
       (Just nn) = Map.lookup (beDVectorID (beNVectorNest nv)) mv 

   liftVM$ VM.opDynamicImm_ VM.ArbbOpGetNesting [nn] [v]
   liftVM$ VM.opDynamicImm_ VM.ArbbOpFlatten [nd] [v] 

   vec <- copyOut (beNVectorData nv) --(Z:.nelems) 
   seg <- copyOut (beNVectorNest nv) --(Z:.nsegs)
   
   return (vec,seg)
   where 
     nelems = sum $ dimList $ beDVectorShape $ beNVectorData nv
     nsegs  = sum $ dimList $ beDVectorShape $ beNVectorNest nv 


-} 

----------------------------------------------------------------------------
-- Read a scalar from the backend
readScalar :: (Data a , IsScalar a, Num a, M.Storable a) => 
              (BEScalar a) -> ArBB a 
readScalar (BEScalar id) = 
    do 
      vm <- S.gets arbbVarMap 
      case Map.lookup id vm of 
         Nothing -> error "readScalar: That scalar does not exist" 
         (Just v) -> liftVM $ VM.readScalar_ v

----------------------------------------------------------------------------
-- Scalar instances  ("uploads" scalars into the backend) 
class Scalar a where 
    mkScalar :: a -> ArBB (BEScalar a) 


#define MKS(ty,load)                    \
  instance Scalar (ty) where {          \
    mkScalar a =                        \
       do {v <- liftVM $ VM.load a;     \
           u <- addVariable v;          \
           return $ BEScalar u } }

MKS(Float,float32_)
MKS(Double,float64_)
MKS(Word8,uint8_)
MKS(Word16,uint16_)
MKS(Word32,uint32_)
MKS(Word64,uint64_)
MKS(Int8,int8_)
MKS(Int16,int16_)
MKS(Int32,int32_)
MKS(Int64,int64_)


                         