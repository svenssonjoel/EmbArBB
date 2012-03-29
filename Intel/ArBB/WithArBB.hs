{-# LANGUAGE ScopedTypeVariables, 
             TypeSynonymInstances, 
             TypeOperators, 
             TypeFamilies, 
             FlexibleContexts, 
             MultiParamTypeClasses,
             FlexibleInstances,
             OverlappingInstances,
             UndecidableInstances
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

import Intel.ArBB.Syntax  
import Intel.ArBB.Vector 
import Intel.ArBB.Types 
import Intel.ArBB.GenArBB
import Intel.ArBB.IsScalar
import Intel.ArBB.Embeddable

----------------------------------------------------------------------------
-- ArBB Monad 

type ArBBState = (Map.Map FunctionName (VM.ConvFunction, [Type], [Type]) , Integer)
                            
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
-- |Executable Function objects
class Executable a b where           
  execute :: Function a b -> a -> ArBB b 
  
{- 
instance (Embeddable (DVector t b) , ArBBIO a,  
          ArBBIO (DVector t b), 
          IsScalar b, V.Storable b) => Executable a (DVector t b) where  
  execute (Function fn) inputs  = 
    do 
      (m,_) <- get 
      case Map.lookup fn m of 
        Nothing -> error "execute: Invalid function" 
        (Just f) -> 
          do 
            ins <- arbbULoad inputs 
          
            [t] <- liftVM$ toArBBType (typeOf (undefined :: (DVector t b)))
          
            g  <- liftVM$ VM.createGlobal_nobind_ t "res" --st "res" 
            y  <- liftVM$ VM.variableFromGlobal_ g

            liftVM$ VM.execute_ f [y] ins
         
            r@(Vector _ res') <- arbbDLoad [y]
                  
            return r
-}           
  
instance (ArBBIO a, 
          ArBBIO b) => Executable a b where 
  execute (Function fn) inputs  = 
    do 
      (m,_) <- get 
      case Map.lookup fn m of 
        Nothing -> error "execute: Invalid function" 
        (Just (f,tins,touts)) -> 
          do 
            ins <- arbbULoad inputs 
            -- [t] <- liftVM$ toArBBType (typeOf (undefined :: b))
            
            ys <- arbbOutVarsFromTypes touts
            -- ys  <- arbbOutVars (undefined :: b)
            -- g  <- liftVM$ VM.createGlobal_nobind_ t "res" --st "res" 
            -- y  <- liftVM$ VM.variableFromGlobal_ g

            liftVM$ VM.execute_ f ys ins
         
            result <- arbbDLoad  ys
            
            return result

arbbOutVarsFromTypes :: [Type] -> ArBB [VM.Variable] 
arbbOutVarsFromTypes [] = return []
arbbOutVarsFromTypes (t:ts) = 
  do 
    [t1',t2'] <- liftVM$ toArBBType t
    -- TODO: THIS IS SKETCHY AND WRONG
    g1 <- liftVM$ VM.createGlobal_nobind_ t1' "res"
    y1 <- liftVM$ VM.variableFromGlobal_ g1
    g2 <- liftVM$ VM.createGlobal_nobind_ t2' "res"
    y2 <- liftVM$ VM.variableFromGlobal_ g2
   
    ys <- arbbOutVarsFromTypes ts
    return (y1 :y2 : ys)
    
class ArBBIO a where 
  arbbULoad :: a -> ArBB [VM.Variable]
  -- TODO: look at again when supporting multiple outputs 
  arbbDLoad :: [VM.Variable] -> ArBB a         
  
  arbbOutVars :: a -> ArBB [VM.Variable]

  
---------------------------------------------------------------------------- 
-- Base
  
-- Scalars
instance ArBBIO Float where 
  arbbULoad a = liftM (:[]) $ liftVM$ VM.float32_ a
  arbbDLoad [v] = liftVM$ VM.readScalar_ v 
  arbbOutVars a = 
    do 
       [t] <- liftVM$ toArBBType (typeOf (undefined :: Float))
            
       g  <- liftVM$ VM.createGlobal_nobind_ t "res" --st "res" 
       y  <- liftVM$ VM.variableFromGlobal_ g
       return [y]


instance ArBBIO Double where 
  arbbULoad a = liftM (:[]) $ liftVM$ VM.float64_ a
  arbbDLoad [v] = liftVM$ VM.readScalar_ v 
  arbbOutVars a = 
    do 
       [t] <- liftVM$ toArBBType (typeOf (undefined :: Double))
            
       g  <- liftVM$ VM.createGlobal_nobind_ t "res" --st "res" 
       y  <- liftVM$ VM.variableFromGlobal_ g
       return [y]


instance ArBBIO Int32 where 
  arbbULoad a = liftM (:[]) $ liftVM$ VM.int32_ a
  arbbDLoad [v] = liftVM$ VM.readScalar_ v 
  arbbOutVars a = 
    do 
       [t] <- liftVM$ toArBBType (typeOf (undefined :: Int32))
            
       g  <- liftVM$ VM.createGlobal_nobind_ t "res" --st "res" 
       y  <- liftVM$ VM.variableFromGlobal_ g
       return [y]

-- Vectors 
instance (V.Storable a, IsScalar a) => ArBBIO (Vector a) where 
  arbbULoad (Vector dat (One n)) = 
    do 
      [st] <- liftVM$ toArBBType (scalarType (undefined :: a)) 
      dt <- liftVM$ VM.getDenseType_ st 1 
      
      let (fptr,n') = V.unsafeToForeignPtr0 dat
          ptr = unsafeForeignPtrToPtr fptr
      
      inb <- liftVM$ VM.createDenseBinding_ (castPtr ptr) 1 [fromIntegral n] [4] 
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
  arbbOutVars a = 
    do 
       [t] <- liftVM$ toArBBType (scalarType (undefined :: a))
       st <- liftVM$ VM.getDenseType_ t 1 
            
       g  <- liftVM$ VM.createGlobal_nobind_ st "res" 
       y  <- liftVM$ VM.variableFromGlobal_ g
       return [y]
  
instance (Num a, V.Storable a, IsScalar a) => ArBBIO (DVector Dim0 a) where 
  arbbULoad (Vector dat Zero) = error "upload: not yet supported" -- undefined
       -- This should be simple
  arbbDLoad [v] = 
    do 
      n  <- liftVM$ VM.readScalar_ v  
                    
      return$ Vector (V.fromList [n]) Zero
  arbbOutVars a = 
    do 
       [t] <- liftVM$ toArBBType (scalarType (undefined :: a))
            
       g  <- liftVM$ VM.createGlobal_nobind_ t "res" --st "res" 
       y  <- liftVM$ VM.variableFromGlobal_ g
       return [y]

----------------------------------------------------------------------------
-- recurse 
instance (ArBBIO a, ArBBIO b) => ArBBIO (a :- b) where 
  arbbULoad (a1 :- rest) = 
    do 
      [v] <- arbbULoad a1   -- correct? 
      vs  <- arbbULoad rest  
      return $ (v:vs) 
  arbbDLoad v = error "arbbDLoad: not supported" 
  arbbOutVars _ = error "arbbOutVars: not supported" 

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
  arbbOutVars (a,b) = 
    do 
       v1 <- arbbOutVars a 
       v2 <- arbbOutVars b 
       
       return$ v1++v2