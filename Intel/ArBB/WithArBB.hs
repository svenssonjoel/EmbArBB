{-# LANGUAGE ScopedTypeVariables, 
             TypeSynonymInstances, 
             TypeOperators, 
             TypeFamilies, 
             FlexibleContexts, 
             MultiParamTypeClasses,
             FlexibleInstances,
             OverlappingInstances,
             UndecidableInstances,
             CPP
             #-}
{- 2012 Joel Svensson -} 

module Intel.ArBB.WithArBB where 

import Control.Monad.State.Strict hiding (liftIO) 
import qualified Data.Map as Map

import qualified Intel.ArbbVM as VM
import qualified Intel.ArbbVM.Convenience as VM

import qualified Data.Vector.Storable as V 
import qualified Data.Vector.Storable.Mutable as M
import Foreign.ForeignPtr 
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils

import Data.Int 
import Data.Word

import Data.IORef

import Intel.ArBB.Syntax  
import Intel.ArBB.Vector 
import Intel.ArBB.Types 
import Intel.ArBB.GenArBB
import Intel.ArBB.IsScalar
import Intel.ArBB.Data.Int
import Intel.ArBB.Data

----------------------------------------------------------------------------
-- ArBB Monad 

-- Keeps track of what functions have been JITed so far
type ArBB a = StateT ArBBState VM.EmitArbb a  

type ArBBState = ( Map.Map FunctionName (VM.ConvFunction, [Type], [Type])
                 , Map.Map Integer VM.Variable -- dVectorID to ArBB vector map
                 , Integer)
                            

-- Add a function to the environment and remember its in-out types for later use. 
addFunction :: FunctionName -> VM.ConvFunction -> [Type] -> [Type] -> ArBB () 
addFunction nom cf tins touts = do 
  (m,v,i) <- get 
  let m' = Map.insert nom (cf,tins,touts)  m 
  put (m',v,i) 

getFunName :: ArBB FunctionName
getFunName = do 
  (m,v,i) <- get 
  put (m,v,i+1) 
  return $ "f" ++ show i 

----------------------------------------------------------------------------
-- Perform IO and VM operations in the ArBB monad.
liftIO :: IO a -> ArBB a 
liftIO = lift . VM.liftIO

liftVM :: VM.EmitArbb a -> ArBB a 
liftVM = lift 

withArBB :: ArBB a -> IO a 
withArBB arbb = 
  do 
    VM.arbbSession$  evalStateT arbb (Map.empty,Map.empty,0)
    
    
----------------------------------------------------------------------------
-- |Get a string representation from a Function. 
serialize :: Function t a -> ArBB String 
serialize (Function fn)  = 
  do 
    (m,_,_) <- get 
    case Map.lookup fn m of 
      Nothing -> error "serialize: Invalid function" 
      (Just (f,tins,touts)) ->  
        do 
          str <- liftVM$ VM.serializeFunction_ f 
          return (VM.getCString str)
          
----------------------------------------------------------------------------
-- | Execute an ArBB function
--execute :: (ArBBIn a, ArBBIO b) =>  Function a b -> a -> ArBB b             
--execute (Function fn) inputs  = 
    -- do 
    --   (m,_) <- get 
    --   case Map.lookup fn m of 
    --     Nothing -> error "execute: Invalid function" 
    --     (Just (f,tins,touts  )) -> 
    --       do 
    --         -- upload the input (creates ArBB variables) 
    --         ins <- arbbUp inputs 
      
    --         -- ys holds the output 
    --         ys <- liftM concat $ liftVM $ mapM typeToArBBGlobalVar touts
          
    --         liftVM$ VM.execute_ f ys ins
         
    --         result <- arbbDLoad  ys
            
    --         return result
            
{-      
execute :: Function a b -> a -> b -> ArBB ()       
execute (Function fn) a b = 
  do 
    (m,_,_) <- get 
    case Map.lookup fn m of 
      Nothing -> error "execute2: Invalid function" 
      (Just (f,tins,touts)) -> 
        do 
          ins <- arbbUp a 
          
          -- if a vector, then allocate space.
          ys <- arbbAlloc b 
          
          liftVM$ VM.execute_ f ys ins 
          
          arbbDown b ys 
          return ()
-}

execute :: (VariableList a, VariableList b)  =>Function a b -> a -> b -> ArBB ()       
execute (Function fn) a b = 
  do 
    (mf,mv,_) <- get 
    case Map.lookup fn mf of 
      Nothing -> error "execute2: Invalid function" 
      (Just (f,tins,touts)) -> 
        do 

          ins <- vlist a 
          outs <- vlist b
          
          liftVM$ VM.execute_ f outs ins 
         
          return ()

class VariableList a where 
    vlist :: a -> ArBB [VM.Variable]

instance VariableList (DVector t a) where 
    vlist v = 
        do 
          (_,mv,_) <- get 
          case Map.lookup (dVectorID v) mv of 
            (Just v) -> return [v] 
            Nothing  -> error "ArBB version of vector not found!"
instance (VariableList t, VariableList rest) => VariableList (t :- rest) where 
    vlist (v :- r) = 
        do
          v' <- vlist v 
          r' <- vlist r 
          return (v' ++ r')

-- useful ?              
freeBindings :: [VM.Binding] -> ArBB ()
freeBindings [] = return () 
freeBindings (b:bs) = 
  do 
    liftVM $ VM.freeBinding_ b
    freeBindings bs

----------------------------------------------------------------------------
finish ::  ArBB ()
finish = liftVM $ VM.finish_   


----------------------------------------------------------------------------
-- Copy data into ArBB 
copyIn :: (Data a, IsScalar a, V.Storable a, Dimensions t) => V.Vector a -> t -> ArBB (DVector t a) 
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

   -- TODO: copy the data! 

   arbbdat <- liftVM$ VM.mapToHost_ v (map fromIntegral dims') VM.ArbbWriteOnlyRange
   liftIO$  copyBytes (castPtr arbbdat) 
                      ptr
                      ((foldl (*) 1 dims') * sizeOf elem) 

   (mf,mv,i) <- get 
   
   let mv' = Map.insert i v mv 
                
   put (mf,mv',i+1)

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
   [st] <- liftVM$ toArBBType (scalarType a)             
   dt <- liftVM$ VM.getDenseType_ st ndims   
   
   g <- liftVM$ VM.createGlobal_nobind_ dt "output"
   v <- liftVM$ VM.variableFromGlobal_ g  
 
   ss <- liftVM$ mapM VM.usize_ dims'
   liftVM$ VM.opDynamicImm_ VM.ArbbOpAlloc [v] ss

   -- TODO: Fill the vector!

   (mf,mv,i) <- get 
   
   let mv' = Map.insert i v mv 
                
   put (mf,mv',i+1)

   return (DVector i dims)
  where 
     -- TODO: There should be some insurance that ndims is 1,2 or 3.
     --       Or a way to handle the higher dimensionalities. 
     ndims = length dims'
     dims = toDim t    
     (Dim dims') = dims -- TODO: FIX FIX 



----------------------------------------------------------------------------
-- Copy data out of ArBB 
copyOut :: (Data a, IsScalar a, V.Storable a, Dimensions t) 
         => DVector t a -> ArBB (V.Vector a) 
copyOut dv = 
  do 
   (vec :: M.IOVector a)  <- liftIO$ M.new $ (foldl (*) 1 dims')

   let (fptr,n') = M.unsafeToForeignPtr0 vec
       ptr       = unsafeForeignPtrToPtr fptr


   (_,mv,_) <- get                    
   -- TODO: STOP CHEATING! 
   let (Just v) = Map.lookup (dVectorID dv) mv
   arbbdat <- liftVM$ VM.mapToHost_ v (map fromIntegral dims') VM.ArbbReadOnlyRange
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



-- BIND
     --  [st] <- liftVM$ toArBBType (scalarType (undefined :: a)) 
     --  dt <- liftVM$ VM.getDenseType_ st 1 
      
     --  let (fptr,n') = V.unsafeToForeignPtr0 dat
     --      ptr = unsafeForeignPtrToPtr fptr
      
     --  inb <- liftVM$ VM.createDenseBinding_ (castPtr ptr) 
     --                                        1 
     --                                        [fromIntegral n] 
     --                                        [fromIntegral $ scalarSize (undefined :: a)] 
     --  gin <- liftVM$ VM.createGlobal_ dt "input" inb 
     --  vin <- liftVM$ VM.variableFromGlobal_ gin
      
     --  return$ [vin]
      
-- COPY
      -- [st] <- liftVM$ toArBBType (scalarType (undefined :: a)) 
      -- dt <- liftVM$ VM.getDenseType_ st 1 
      
      -- gout <- liftVM$ VM.createGlobal_nobind_ dt "output"
      -- vout <- liftVM$ VM.variableFromGlobal_ gout 
      
      -- s <- liftVM$ VM.usize_ n 
      -- liftVM$ VM.opDynamicImm_ VM.ArbbOpAlloc [vout] [s] 
      -- return [vout]

{- 
----------------------------------------------------------------------------    
-- ArBBIn, ArBBOut : used by execute2            
class ArBBIn a where                  
  arbbUp :: a -> ArBB [VM.Variable] 

class ArBBOut a where 
  -- use the list of variables to access data 
  -- to store into the a (will be a mutable of some kind) 
  arbbDown  :: a -> [VM.Variable] -> ArBB () 
  arbbAlloc :: a -> ArBB [VM.Variable]
  
  
-- CPP Hackery 
#define UpScalar(ty,load)                      \
  instance ArBBIn (ty) where {                 \
    arbbUp a = liftM (:[]) $ liftVM$ VM.load a}
#define DownScalar(ty,typ)                                 \
  instance ArBBOut (ty) where {                            \
    arbbDown i [v] = do {val <-  liftVM (VM.readScalar_ v);\
                         liftIO (writeIORef i val)};       \
    arbbAlloc a = liftVM $ typeToArBBGlobalVar (typeOf (undefined :: typ)); } 


UpScalar(Int,int64_)  -- fix for 64/32 bit archs
UpScalar(Int8,int8_)
UpScalar(Int16,int16_)
UpScalar(Int32,int32_)
UpScalar(Int64,int64_)
UpScalar(Word,uint64_) -- fir for 64/32 bit archs
UpScalar(Word8,uint8_)
UpScalar(Word16,uint16_)
UpScalar(Word32,uint32_)
UpScalar(Word64,uint64_)
UpScalar(Float,float32_)
UpScalar(Double,float64_)

DownScalar(IORef Int,Int)
DownScalar(IORef Int8,Int8)
DownScalar(IORef Int16,Int16)
DownScalar(IORef Int32,Int32)
DownScalar(IORef Int64,Int64)
DownScalar(IORef Word,Word) 
DownScalar(IORef Word8,Word8) 
DownScalar(IORef Word16,Word16) 
DownScalar(IORef Word32,Word32) 
DownScalar(IORef Word64,Word64) 
DownScalar(IORef Float,Float)
DownScalar(IORef Double,Double)

instance ArBBIn () where 
  arbbUp () = return []

instance (V.Storable a, IsScalar a) => ArBBIn (DVector Dim1 a) where 
  arbbUp (Vector dat (One n)) = 
    do 
      [st] <- liftVM$ toArBBType (scalarType (undefined :: a)) 
      dt <- liftVM$ VM.getDenseType_ st 1 
      
      let (fptr,n') = V.unsafeToForeignPtr0 dat
          ptr = unsafeForeignPtrToPtr fptr
      
      inb <- liftVM$ VM.createDenseBinding_ (castPtr ptr) 
                                            1 
                                            [fromIntegral n] 
                                            [fromIntegral $ scalarSize (undefined :: a)] 
      gin <- liftVM$ VM.createGlobal_ dt "input" inb 
      vin <- liftVM$ VM.variableFromGlobal_ gin
      
      return$ [vin]
      
instance (V.Storable a, IsScalar a) => ArBBIn (DVector Dim2 a) where 
  arbbUp (Vector dat (Two n1 n2)) = 
    do 
      [st] <- liftVM$ toArBBType (scalarType (undefined :: a)) 
      dt <- liftVM$ VM.getDenseType_ st 2
      
      let (fptr,n') = V.unsafeToForeignPtr0 dat
          ptr = unsafeForeignPtrToPtr fptr
      
      inb <- liftVM$ VM.createDenseBinding_ (castPtr ptr) 
                                            2
                                            [fromIntegral n1,
                                             fromIntegral n2] 
                                            [s,(fromIntegral n1)*s] 
      gin <- liftVM$ VM.createGlobal_ dt "input" inb 
      vin <- liftVM$ VM.variableFromGlobal_ gin
      
      return$ [vin]
        where 
          s = fromIntegral $ scalarSize (undefined :: a)
instance (V.Storable a, IsScalar a) => ArBBIn (DVector Dim3 a) where 
  arbbUp (Vector dat (Three n1 n2 n3)) = 
    do 
      [st] <- liftVM$ toArBBType (scalarType (undefined :: a)) 
      dt <- liftVM$ VM.getDenseType_ st 3
      
      let (fptr,n') = V.unsafeToForeignPtr0 dat
          ptr = unsafeForeignPtrToPtr fptr
      
      inb <- liftVM$ VM.createDenseBinding_ (castPtr ptr) 
                                            3
                                            [fromIntegral n1, 
                                             fromIntegral n2,
                                             fromIntegral n3] 
                                            [ s
                                            , fromIntegral n1 * s
                                            , fromIntegral (n1 * n2) * s                                              
                                            ] 
      gin <- liftVM$ VM.createGlobal_ dt "input" inb 
      vin <- liftVM$ VM.variableFromGlobal_ gin
      
      return$ [vin]
        where 
          s = fromIntegral $ scalarSize (undefined :: a)

instance (ArBBIn a, ArBBIn b) => ArBBIn (a :- b) where 
  arbbUp (a :- b) = 
    do 
      a' <- arbbUp a 
      b' <- arbbUp b
      return $ a' ++ b'

      
---------------------------------------------------------------------------- 
-- Out instances 
instance (Data a, V.Storable a, IsScalar a) => ArBBOut (MDVector Dim1 a) where 
  arbbAlloc (MVector _ (One n)) = 
    do 
      [st] <- liftVM$ toArBBType (scalarType (undefined :: a)) 
      dt <- liftVM$ VM.getDenseType_ st 1 
      
      gout <- liftVM$ VM.createGlobal_nobind_ dt "output"
      vout <- liftVM$ VM.variableFromGlobal_ gout 
      
      s <- liftVM$ VM.usize_ n 
      liftVM$ VM.opDynamicImm_ VM.ArbbOpAlloc [vout] [s] 
      return [vout]
    
  arbbDown  (MVector dat (One n)) [v] = 
    do 
      arbbdat <- liftVM$ VM.mapToHost_ v [fromIntegral n] VM.ArbbReadOnlyRange 
      let (ptr,i) = M.unsafeToForeignPtr0 dat
      liftIO$  copyBytes (unsafeForeignPtrToPtr ptr) (castPtr arbbdat) (n * sizeOf (undefined :: a)) 


instance (Data a, V.Storable a, IsScalar a) => ArBBOut (MDVector Dim2 a) where 
  arbbAlloc (MVector _ (Two n1 n2)) = 
    do 
      [st] <- liftVM$ toArBBType (scalarType (undefined :: a)) 
      dt <- liftVM$ VM.getDenseType_ st 2 
      
      gout <- liftVM$ VM.createGlobal_nobind_ dt "output"
      vout <- liftVM$ VM.variableFromGlobal_ gout 
      
      s1 <- liftVM$ VM.usize_ n1 
      s2 <- liftVM$ VM.usize_ n2
      liftVM$ VM.opDynamicImm_ VM.ArbbOpAlloc [vout] [s1,s2] -- correct ?
      return [vout]
    
  arbbDown  (MVector dat (Two n1 n2)) [v] = 
    do 
      arbbdat <- liftVM$ VM.mapToHost_ v [fromIntegral (n1 * n2)] VM.ArbbReadOnlyRange 
      let (ptr,i) = M.unsafeToForeignPtr0 dat
      liftIO$  copyBytes (unsafeForeignPtrToPtr ptr) 
                         (castPtr arbbdat) 
                         (n1 * n2 * sizeOf (undefined :: a)) 


instance (Data a, V.Storable a, IsScalar a) => ArBBOut (MDVector Dim3 a) where 
  arbbAlloc (MVector _ (Three n1 n2 n3)) = 
    do 
      [st] <- liftVM$ toArBBType (scalarType (undefined :: a)) 
      dt <- liftVM$ VM.getDenseType_ st 3
      
      gout <- liftVM$ VM.createGlobal_nobind_ dt "output"
      vout <- liftVM$ VM.variableFromGlobal_ gout 
      
      s1 <- liftVM$ VM.usize_ n1 
      s2 <- liftVM$ VM.usize_ n2
      s3 <- liftVM$ VM.usize_ n3
      liftVM$ VM.opDynamicImm_ VM.ArbbOpAlloc [vout] [s1,s2,s3] -- Correct ?  
      return [vout]
    
  arbbDown  (MVector dat (Three n1 n2 n3)) [v] = 
    do 
      arbbdat <- liftVM$ VM.mapToHost_ v 
                                       [fromIntegral (n1 * n2 * n3)] 
                                       VM.ArbbReadOnlyRange 
      let (ptr,i) = M.unsafeToForeignPtr0 dat
      liftIO$  copyBytes (unsafeForeignPtrToPtr ptr) 
                         (castPtr arbbdat) 
                         (n1 * n2 * n3 * sizeOf (undefined :: a)) 
  
instance (ArBBOut a, ArBBOut b) => ArBBOut (a :- b) where 
  arbbAlloc (a :- b) = 
    do 
      v1 <- arbbAlloc a 
      v2 <- arbbAlloc b
      return $ v1++v2
  arbbDown (a :- b) [v1,v2] = 
    do 
      arbbDown a [v1] 
      arbbDown b [v2]


  
----------------------------------------------------------------------------                 
-- ArBBIO : used by execute
class ArBBIO a where 
  arbbULoad :: a -> ArBB [VM.Variable]
  -- TODO: look at again when supporting multiple outputs 
  arbbDLoad :: [VM.Variable] -> ArBB a         

instance ArBBIO () where 
  arbbULoad _ = return []
  arbbDLoad _ = return () 
  
---------------------------------------------------------------------------- 
-- Base
  
-- Scalars
  
#define ArBBScalar(ty,load)                        \
  instance ArBBIO ty where {                       \
    arbbULoad a = liftM (:[]) $ liftVM$ VM.load a; \
    arbbDLoad [v] = liftVM$ VM.readScalar_ v } 

ArBBScalar(Int8,int8_)
ArBBScalar(Int16,int16_)
ArBBScalar(Int32,int32_)
ArBBScalar(Int64,int64_)
ArBBScalar(Word8,uint8_)
ArBBScalar(Word16,uint16_)
ArBBScalar(Word32,uint32_)
ArBBScalar(Word64,uint64_)
ArBBScalar(Float,float32_)
ArBBScalar(Double,float64_)
-- ArBBScalar(Bool,bool_) -- a special case 
ArBBScalar(USize,usize_) 
ArBBScalar(ISize,isize_)

----------------------------------------------------------------------------
-- Vectors 
instance (V.Storable a, IsScalar a) => ArBBIO (Vector a) where 
  arbbULoad (Vector dat (One n)) = 
    do 
      [st] <- liftVM$ toArBBType (scalarType (undefined :: a)) 
      dt <- liftVM$ VM.getDenseType_ st 1 
      
      let (fptr,n') = V.unsafeToForeignPtr0 dat
          ptr = unsafeForeignPtrToPtr fptr
      
      inb <- liftVM$ VM.createDenseBinding_ (castPtr ptr) 
                                            1 
                                            [fromIntegral n] 
                                            [fromIntegral $ scalarSize (undefined :: a)] 
      gin <- liftVM$ VM.createGlobal_ dt "input" inb 
      vin <- liftVM$ VM.variableFromGlobal_ gin
      
      return$ [vin]
  arbbDLoad [v] = 
    do 
      [st] <- liftVM$ toArBBType (scalarType (undefined :: a)) 
      dt <- liftVM$ VM.getDenseType_ st 1 
      
    
      size_t <- liftVM$ VM.getScalarType_ VM.ArbbUsize 
      gs  <- liftVM$ VM.createGlobal_nobind_ size_t "size"
      s  <- liftVM$ VM.variableFromGlobal_ gs
      
      -- Read size of vector from ArBB using an immediate op
      liftVM$ VM.opImm_ VM.ArbbOpLength [s] [v]
      
      n :: Int32 <- liftVM$ VM.readScalar_ s  
                    
      -- now load that number of elements from the VM 
      ptr <- liftVM$ VM.mapToHost_ v [1] VM.ArbbReadOnlyRange
      dat <- liftIO$ peekArray (fromIntegral n) (castPtr ptr) 
      
      return$ Vector (V.fromList dat) (One (fromIntegral n))
 
-- Dim0 is a special case   
instance (Num a, V.Storable a, IsScalar a) => ArBBIO (DVector Dim0 a) where 
  arbbULoad (Vector dat Zero) = error "upload: not yet supported" 
       -- This should be simple
  arbbDLoad [v] = 
    do 
      n  <- liftVM$ VM.readScalar_ v  
                    
      return$ Vector (V.fromList [n]) Zero
      
instance (V.Storable a, IsScalar a) => ArBBIO (DVector Dim2 a) where 
  arbbULoad (Vector dat (Two n1 n2)) = 
    do 
      [st] <- liftVM$ toArBBType (scalarType (undefined :: a)) 
      dt <- liftVM$ VM.getDenseType_ st 2 
      
      let (fptr,n') = V.unsafeToForeignPtr0 dat
          ptr = unsafeForeignPtrToPtr fptr
      
      inb <- liftVM$ VM.createDenseBinding_ (castPtr ptr) 
                                            2 
                                            [fromIntegral n1,fromIntegral n2] 
                                            [s,s*(fromIntegral n1)]
      gin <- liftVM$ VM.createGlobal_ dt "input" inb 
      vin <- liftVM$ VM.variableFromGlobal_ gin
      --liftIO$ putStrLn$ show s 
      --liftIO$ putStrLn$ show n1 
      --liftIO$ putStrLn$ show n2
      
      return$ [vin]
    where   
      s = fromIntegral $ scalarSize (undefined :: a) 
  arbbDLoad [v] = 
    do 
      [st] <- liftVM$ toArBBType (scalarType (undefined :: a)) 
      dt <- liftVM$ VM.getDenseType_ st 2
      
    
      size_t <- liftVM$ VM.getScalarType_ VM.ArbbUsize 
      gs1  <- liftVM$ VM.createGlobal_nobind_ size_t "size"
      s1  <- liftVM$ VM.variableFromGlobal_ gs1
      gs2  <- liftVM$ VM.createGlobal_nobind_ size_t "size"
      s2  <- liftVM$ VM.variableFromGlobal_ gs2
      
      -- Read size of vector from ArBB using an immediate op
      liftVM$ VM.opImm_ VM.ArbbOpGetNcols [s1] [v]
      liftVM$ VM.opImm_ VM.ArbbOpGetNrows [s2] [v]
      
      
      n1 :: Int64 <- liftVM$ VM.readScalar_ s1  
      n2 :: Int64 <- liftVM$ VM.readScalar_ s2     
      -- liftIO$ putStrLn$ show n1
      -- liftIO$ putStrLn$ show n2

      -- now load that number of elements from the VM 
      ptr <- liftVM$ VM.mapToHost_ v [1] VM.ArbbReadOnlyRange
      dat <- liftIO$ peekArray (fromIntegral (n1*n2)) (castPtr ptr) 
      
      return$ Vector (V.fromList dat) (Two (fromIntegral n1) (fromIntegral n2))
 

----------------------------------------------------------------------------
-- recurse 
instance (ArBBIO a, ArBBIO b) => ArBBIO (a :- b) where 
  arbbULoad (a1 :- rest) = 
    do 
      [v] <- arbbULoad a1   -- correct? 
      vs  <- arbbULoad rest  
      return (v:vs) 
  -- TODO: Fix this. Again, probably needs to maintain some structure.
  --       as long as the type 'a' is not a tuple this should be ok. 
  arbbDLoad (a1:rest) =
    do 
      r1 <- arbbDLoad [a1] 
      rs <- arbbDLoad rest
      return (r1 :- rs)  
 

----------------------------------------------------------------------------
-- Tuples 
instance (ArBBIO a, ArBBIO b) => ArBBIO (a,b) where 
  arbbULoad (a,b) = 
    do 
      v1 <- arbbULoad a 
      v2 <- arbbULoad b 
      return $ v1 ++ v2
  arbbDLoad [a,b] = 
    do 
      v1 <- arbbDLoad [a]
      v2 <- arbbDLoad [b] 

      return (v1,v2)
 
  
instance (ArBBIO a, ArBBIO b, ArBBIO c) => ArBBIO (a,b,c) where 
  arbbULoad (a,b,c) = 
    do 
      v1 <- arbbULoad a 
      v2 <- arbbULoad b 
      v3 <- arbbULoad c 
      return $ v1 ++ v2 ++ v3
  arbbDLoad [a,b,c] = 
    do 
      v1 <- arbbDLoad [a]
      v2 <- arbbDLoad [b] 
      v3 <- arbbDLoad [c]       

      return (v1,v2,v3)
-}    