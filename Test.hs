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
import Control.Monad.State hiding (liftIO)

import Prelude as P


addconst :: Exp Word32 -> Exp (DVector Dim1 Word32) -> Exp (DVector Dim1 Word32) 
addconst s v = v + ss 
    where 
      ss = constVector s (Lang.length v) 


plus75 :: Exp Word32 -> Exp Word32 
plus75 x = x + 75

mapTest :: Exp (DVector Dim1 Word32) -> Exp (DVector Dim1 Word32) 
mapTest v = Lang.map plus75 v 


test4 = 
    withArBB $ do
      f <- capture mapTest

      -- Show a string representation of a function     
      str <- serialize f 
      liftIO$ putStrLn str

      -- Turn a normal Data.Vector into a backend-vector 
      x <- copyIn (V.fromList [1..10::Word32]) (Z :. 10)
      
      -- Create a new vector in the backend for the result. )
      -- backend vectors are mutable.
      r1 <- new (Z :. 10) 0
      
      execute f x r1

      -- This freezes a mutable DVector into a normal Data.Vector.                                  
      r <- copyOut r1
          
      liftIO$ putStrLn $ show r 


test5 = 
    withArBB $ do 
      f <- capture numr
      g <- capture numc
      
      str <- serialize f 
      liftIO$ putStrLn str
      
      x <- copyIn (V.fromList [1..10::Word32]) (Z :. 2 :. 5)
      r1 <- new (Z :. 1) 0 
      r2 <- new (Z :. 1) 0 

      execute f x r1 
      execute g x r2 

      r <- copyOut r1
      c <- copyOut r2

      liftIO$ putStrLn $ "Rows: "++ show r ++ " || Cols: " ++ show c
          
    where 
      numr :: Exp (DVector Dim2 Word32) -> Exp (DVector Dim1 USize)
      numr v = constVector n 1 
          where n = getNRows v

      numc :: Exp (DVector Dim2 Word32) -> Exp (DVector Dim1 USize)
      numc v = constVector n 1 
          where n = getNCols v


