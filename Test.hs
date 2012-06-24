
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
      g <- capture segFlat      
      h <- capture indexS
      i <- capture tRed

      str <- serialize f 
      liftIO$ putStrLn str

      str <- serialize g 
      liftIO$ putStrLn str
      
      str <- serialize h
      liftIO$ putStrLn str

      str <- serialize i
      liftIO$ putStrLn str
      
      x <- copyIn (V.fromList [1..10::Word32]) (Z:.10)
      s <- copyIn (V.fromList [-1,-1,-1,-1,-1,1,1,1,1,1]) (Z:.10) 
      r1 <- new (Z :. 5) 0
      r2 <- new (Z :. 10) 0     
      r3 <- mkScalar 0
      r4 <- new (Z :. 2) 0 
      (r5 :: BEDVector Dim2 Word32)  <- new (Z :. 2 :. 5) 102 

      execute f (x :- s) r1 

      execute g (x :- s) r2 

      execute h (x :- s) r3
              
      execute i (x :- s) r4 

      ra <- copyOut r1
      rb <- copyOut r2
      rc <- readScalar r3
      rd <- copyOut r4
      re <- copyOut r5

      liftIO$ putStrLn $ show ra
      liftIO$ putStrLn $ show rb
      liftIO$ putStrLn $ show rc 
      liftIO$ putStrLn $ show rd 
      liftIO$ putStrLn $ show re 
      
          
    where 
      getSeg :: Exp (DVector Dim1 Word32) 
              -> Exp (DVector Dim1 ISize) 
              -> (Exp (DVector Dim1 Word32)) -- ,Exp (DVector Dim1 Word32))
      getSeg v s = segment res 0 + segment res 1
          where 
            res :: Exp (NVector Word32)
            res = split v s

      segFlat :: Exp (DVector Dim1 Word32) 
               -> Exp (DVector Dim1 ISize) 
               -> (Exp (DVector Dim1 Word32))
      segFlat v s = flattenSeg(split v s) 

      indexS :: Exp (DVector Dim1 Word32) 
               -> Exp (DVector Dim1 ISize) 
               -> Exp Word32
      indexS v s = indexSeg (split v s) 1 0 
 
      tRed :: Exp (DVector Dim1 Word32) 
               -> Exp (DVector Dim1 ISize) 
               -> Exp (DVector Dim1 Word32)
      tRed  v s = addReduceSeg (split v s)


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


{-             
testSeg = 
    withArBB $ do
      f <- capture getSeg

      -- Show a string representation of a function     
      str <- serialize f 
      liftIO$ putStrLn str

      -- Turn a normal Data.Vector into a backend-vector 
      x <- copyIn (V.fromList [1..10::Word32]) (Z:.10)
      i <- copyIn (V.fromList [-1,-1,-1,-1,-1,1,1,1,1,1]) (Z:.10) 
      -- Create a new vector in the backend for the result. )
      -- backend vectors are mutable.
      liftIO$ putStrLn "beforeNew"
      r1  <- newNested 10 2 0
      liftIO$ putStrLn "before"
      execute f (x :- i) r1

      -- This freezes a mutable DVector into a normal Data.Vector.                                  
      (rd,rn) <- copyOutNested r1
          
      liftIO$ putStrLn $ show rd
      liftIO$ putStrLn $ show rn
      
    where
      getSeg :: Exp (DVector Dim1 Word32) 
              -> Exp (DVector Dim1 ISize) 
              -> (Exp (NVector Word32))
      getSeg v s = split v s 
-}