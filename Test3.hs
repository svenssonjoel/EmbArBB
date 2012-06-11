{-# LANGUAGE ScopedTypeVariables,
             TypeOperators #-} 

{- 2012 Joel Svensson -} 

module Test where 

--import Intel.ArBB as ArBB
import qualified Intel.ArbbVM as VM
import qualified Intel.ArbbVM.Convenience as VM

--import Intel.ArBB.WithArBB
import Intel.ArBB.Vector
import Intel.ArBB.Data.Int

import Intel.ArBB.BackendExperiment2 
import Intel.ArBB.Language as Lang
import Intel.ArBB.Syntax
import Intel.ArBB.DAG

import qualified Data.Vector.Storable as V 

import Foreign.Marshal.Array
import qualified Foreign.Marshal.Utils as Utils
import Foreign.Ptr

import Data.Int 
import Data.Word
import Data.IORef

import qualified Data.Map as Map
import Control.Monad.State hiding (liftIO)

import Prelude as P


addconst :: Exp Word32 -> Exp (DVector Dim1 Word32) -> Exp (DVector Dim1 Word32) 
addconst s v = v + ss 
    where 
      ss = constVector s (Lang.length v) 

test1 :: IO [NodeID]
test1 = runR (reify addconst)
test2 = 
    do 
      (a,b) <- rR (reify addconst)
      return b
-- test2 = runArBB test1 

-- test2 = runBackend (test1 :: ArBBBackend [NodeID])
   


{- 
test1 = 
  withArBB $
    do 
      
      f <- capture (addconst (100 :: Exp Word32)) 
          
      str <- serialize f 
      liftIO $ putStrLn str
   
      x  <- copyIn (V.fromList [1..10::Word32]) ((10::Int) :. Z )
      (r1 :: DVector (Int :. Z) Word32) <- new ((10::Int) :. Z) 0 
      
      execute f x r1 
  
      r <- copyOut r1

      liftIO$ putStrLn$ show r
      return f
  
main = putStrLn "tests"
-} 