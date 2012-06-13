{-# LANGUAGE ScopedTypeVariables,
             TypeOperators #-} 

{- 2012 Joel Svensson -} 

module Test where 


import qualified Intel.ArbbVM as VM
import qualified Intel.ArbbVM.Convenience as VM


import Intel.ArBB.Vector
import Intel.ArBB.Data.Int

import Intel.ArBB.BackendExperiment 
import Intel.ArBB.Language as Lang
import Intel.ArBB.Syntax
import Intel.ArBB.DAG

import Intel.ArBB.Backend.ArBB

import qualified Data.Vector.Storable as V 

import Foreign.Marshal.Array
import qualified Foreign.Marshal.Utils as Utils
import Foreign.Ptr

import Data.Int 
import Data.Word
import Data.IORef

import qualified Data.Map as Map
import Control.Monad.State -- hiding (liftIO)

import Prelude as P


addconst :: Exp Word32 -> Exp (DVector Dim1 Word32) -> Exp (DVector Dim1 Word32) 
addconst s v = v + ss 
    where 
      ss = constVector s (Lang.length v) 


plus75 :: Exp Word32 -> Exp Word32 
plus75 x = x + 75

mapTest :: Exp (DVector Dim1 Word32) -> Exp (DVector Dim1 Word32) 
mapTest v = Lang.map plus75 v 

--test1 = runR (reify addconst)

--test2 =  runArBBBackendAll test1 

test3 = 
    withArBB $ do
      f <- capture (addconst (100 :: Exp Word32))

      -- Show a string representation of a function     
      str <- serialize f 
      liftIO$ putStrLn str

      -- Turn a normal Data.Vector into a DVector (an EmbArBB dense vector) 
      x <- copyIn (V.fromList [1..10::Word32]) ((10::Int) :. Z)
      
      -- Create a new DVector for the result. (DVectors in here 
      -- are mutable!)
      (r1 :: DVector (Int :. Z) Word32) <- new ((10::Int) :. Z) 1
      
      execute f x r1

      -- you can if you like execute f again using r1 as both input and output. 
      execute f r1 r1
          
      -- This freezes a mutable DVector into a normal Data.Vector.                                  
      r <- copyOut r1
          
      liftIO$ putStrLn $ show r 



test4 = 
    withArBB $ do
      f <- capture mapTest

      -- Show a string representation of a function     
      str <- serialize f 
      liftIO$ putStrLn str

      -- Turn a normal Data.Vector into a DVector (an EmbArBB dense vector) 
      x <- copyIn (V.fromList [1..10::Word32]) ((10::Int) :. Z)
      
      -- Create a new DVector for the result. (DVectors in here 
      -- are mutable! Is that awkward?)
      (r1 :: DVector (Int :. Z) Word32) <- new ((10::Int) :. Z) 0
      
      execute f x r1

      -- This freezes a mutable DVector into a normal Data.Vector.                                  
      r <- copyOut r1
          
      liftIO$ putStrLn $ show r 



