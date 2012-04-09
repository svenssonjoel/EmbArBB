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
import Foreign.ForeignPtr 
import Foreign.Ptr
import Foreign.Marshal.Array

import Data.Int 
import Data.Word

import Intel.ArBB.Syntax  
import Intel.ArBB.Vector 
import Intel.ArBB.Types 
import Intel.ArBB.GenArBB
import Intel.ArBB.IsScalar
import Intel.ArBB.Data.Int

----------------------------------------------------------------------------
-- ArBB Monad 

type ArBBState = (Map.Map FunctionName (VM.ConvFunction, [Type], [Type]) , Integer)
                            

-- Add a function to the environment and remember its in-out types for later use. 
addFunction :: FunctionName -> VM.ConvFunction -> [Type] -> [Type] -> ArBB () 
addFunction nom cf tins touts = do 
  (m,i) <- get 
  let m' = Map.insert nom (cf,tins,touts)  m 
  put (m',i) 

getFunName :: ArBB FunctionName
getFunName = do 
  (m,i) <- get 
  put (m,i+1) 
  return $ "f" ++ show i 

-- Keeps track of what functions have been JITed so far
type ArBB a = StateT ArBBState VM.EmitArbb a  

liftIO :: IO a -> ArBB a 
liftIO = lift . VM.liftIO

liftVM :: VM.EmitArbb a -> ArBB a 
liftVM = lift 

withArBB :: ArBB a -> IO a 
withArBB arbb = 
  do 
    VM.arbbSession$  evalStateT arbb (Map.empty,0)
    
    
----------------------------------------------------------------------------
-- |Get a string representation from a Function. 
serialize :: Function t a -> ArBB String 
serialize (Function fn)  = 
  do 
    (m,_) <- get 
    case Map.lookup fn m of 
      Nothing -> error "serialize: Invalid function" 
      (Just (f,tins,touts)) ->  
        do 
          str <- liftVM$ VM.serializeFunction_ f 
          return (VM.getCString str)
          
----------------------------------------------------------------------------
-- | Execute an ArBB function
execute :: (ArBBIO a, ArBBIO b) =>  Function a b -> a -> ArBB b             
execute (Function fn) inputs  = 
    do 
      (m,_) <- get 
      case Map.lookup fn m of 
        Nothing -> error "execute: Invalid function" 
        (Just (f,tins,touts  )) -> 
          do 
            ins <- arbbULoad inputs 
      
            -- TODO: Big changes here when user needs to supply result vectors.
            ys <- liftM concat $ liftVM $ mapM typeToArBBGlobalVar touts
            --y1 <- liftVM $ denseTypeSizeToGlobalVar t1 40
            --ys <- liftVM$ typeToArBBGlobalVar t2

            liftVM$ VM.execute_ f ys ins
         
            result <- arbbDLoad  ys
            
            return result
      
    
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
#define ArBBScalar(ty,load) instance ArBBIO ty where {arbbULoad a = liftM (:[]) $ liftVM$ VM.load a; arbbDLoad [v] = liftVM$ VM.readScalar_ v } 

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

-- TODO: way to remove the IsScalar requirement (Big change !!) 

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
      return $ (v:vs) 
  arbbDLoad v = error "arbbDLoad: not supported" 
 

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
 