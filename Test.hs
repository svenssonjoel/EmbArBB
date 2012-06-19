
{-# LANGUAGE ScopedTypeVariables,
             TypeOperators #-} 

{- 2012 Joel Svensson -} 

module Test where 


import qualified Intel.ArbbVM as VM
import qualified Intel.ArbbVM.Convenience as VM


import Intel.ArBB.Vector
import Intel.ArBB.Data.Int


import Intel.ArBB
import Intel.ArBB.Language as Lang
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



test6 = 
    withArBB $ do 
      f <- capture getSeg
      
      str <- serialize f 
      liftIO$ putStrLn str
      
      x <- copyIn (V.fromList [1..10::Word32]) (Z:.10)
      s <- copyIn (V.fromList [-1,-1,-1,-1,-1,1,1,1,1,1]) (Z:.10) 
      r1 <- new (Z :. 5) 0 
      -- r2 <- new (Z :. 5) 0 

      execute f (x :- s)  r1 -- (r1 :- r2) 

      ra <- copyOut r1
--      rb <- copyOut r2

      liftIO$ putStrLn $ show ra
  --    liftIO$ putStrLn $ show rb
          
    where 
      getSeg :: Exp (DVector Dim1 Word32) 
              -> Exp (DVector Dim1 ISize) 
              -> (Exp (DVector Dim1 Word32)) -- ,Exp (DVector Dim1 Word32))
      getSeg v s = segment res 0 + segment res 1
          where 
            -- res :: Exp (NVector Word32)
            res = split v s




sharing :: Exp (DVector Dim1 Word32) -> Exp (DVector Dim1 Word32) 
sharing v1 =  v1'''
    where 
      v1' = v1+v1
      v1'' = v1' + v1' 
      v1''' = (v1'' + v1'') + v1''

testSharing = 
    withArBB $ do
      f <- capture sharing

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
