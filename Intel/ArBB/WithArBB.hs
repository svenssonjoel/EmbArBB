{-# LANGUAGE TypeSynonymInstances, 
             TypeOperators, 
             ScopedTypeVariables, 
             FlexibleContexts #-}
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

import Intel.ArBB.Syntax  -- (FunctionName)
import Intel.ArBB.Vector 
import Intel.ArBB.Types 
import Intel.ArBB.GenArBB
import Intel.ArBB.IsScalar

----------------------------------------------------------------------------
--

type ArBBState = (Map.Map FunctionName VM.ConvFunction, Integer)
                            
addFunction :: FunctionName -> VM.ConvFunction -> ArBB () 
addFunction nom cf = do 
  (m,i) <- get 
  let m' = Map.insert nom cf m 
  put (m',i) 

getFunName :: ArBB FunctionName
getFunName = do 
  (m,i) <- get 
  put (m,i+1) 
  return $ "f" ++ show i 

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
-- 

serialize :: Function t a -> ArBB String 
serialize (Function fn)  = 
  do 
    (m,_) <- get 
    case Map.lookup fn m of 
      Nothing -> error "serialize: Invalid function" 
      (Just f) ->  
        do 
          str <- liftVM$ VM.serializeFunction_ f 
          return (VM.getCString str)
          
          
----------------------------------------------------------------------------
--

-- TODO: Lots of cheating going on here ! iron it out. 
-- TODO: I cannot get this function to have the type I want. 
--       Is that even possible ? 
-- execute :: (ArBBIO a, ArBBIO t)  => Function t a -> t -> ArBB a 
execute :: (IsScalar a, V.Storable a, ArBBIO t, ArBBIO (DVector t0 a))  
           => Function t (DVector t0 a) -> t -> ArBB (DVector t0 a) 
execute (Function fn) inputs  = 
  do 
    (m,_) <- get 
    case Map.lookup fn m of 
      Nothing -> error "execute: Invalid function" 
      (Just f) -> 
        do 
          ins <- arbbULoad inputs 
          
          st <- liftVM$ VM.getScalarType_ VM.ArbbF32
          dt <- liftVM$ VM.getDenseType_ st 1  
          
          g  <- liftVM$ VM.createGlobal_nobind_ dt "res" --st "res" 
          y  <- liftVM$ VM.variableFromGlobal_ g

          liftVM$ VM.execute_ f [y] ins
         
          r@(Vector _ res') <- arbbDLoad y
                  
          return r
          
class ArBBIO a where 
  arbbULoad :: a -> ArBB [VM.Variable]
  -- TODO: look at again when supporting multiple outputs 
  arbbDLoad :: VM.Variable -> ArBB a         
  
instance (V.Storable a, IsScalar a) => ArBBIO (Vector a) where 
  arbbULoad (Vector dat (One n)) = 
    do 
      st <- liftVM$ toArBBType (scalarType (undefined :: a)) 
      dt <- liftVM$ VM.getDenseType_ st 1 
      
      let (fptr,n') = V.unsafeToForeignPtr0 dat
          ptr = unsafeForeignPtrToPtr fptr
      
      inb <- liftVM$ VM.createDenseBinding_ (castPtr ptr) 1 [fromIntegral n] [4] 
      gin <- liftVM$ VM.createGlobal_ dt "input" inb 
      vin <- liftVM$ VM.variableFromGlobal_ gin
      
      return$ [vin]
  arbbDLoad v = 
    do 
      st <- liftVM$ toArBBType (scalarType (undefined :: a)) 
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

-- TODO: The dimensions of the result can be grabbed from ArbbVM
--       using the opLength etc 
arbbDLoad' v = 
  do 
    --st <- liftVM$ toArBBType (scalarType (undefined :: a)) 
    st <- liftVM$ VM.getScalarType_ VM.ArbbF32
    dt <- liftVM$ VM.getDenseType_ st 1  
    
    ptr <- liftVM$ VM.mapToHost_ v [1] VM.ArbbReadOnlyRange
    dat <- liftIO$ peekArray 3 (castPtr ptr :: Ptr Float) 
    liftIO$ putStrLn$ show dat 


  
instance (ArBBIO a, ArBBIO b) => ArBBIO (a :- b) where 
  arbbULoad (a1 :- rest) = 
    do 
      [v] <- arbbULoad a1   -- correct? 
      vs  <- arbbULoad rest  
      return $ (v:vs) 
  arbbDLoad v = undefined  -- many outputs? How ? 
