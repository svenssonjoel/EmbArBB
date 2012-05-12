{-# LANGUAGE ScopedTypeVariables #-} 

{- 2012 Joel Svensson -} 

module Test where 

import Intel.ArBB 

import qualified Data.Vector.Storable as V 

import Data.Int
import Data.Word


-- TODO: Way to locally create the "local" vector.. 
-- TODO: The c++ version uses some kind of trick where a 1 is turned into an array of suitable size 
--       add_merge(1,input,256) ... 


histogram :: Exp (Vector Int32) -> Exp (Vector USize)  -> Exp (Vector Int32)
histogram local input = addMerge local input 256 
 

testHist = 
  withArBB $ 
  do 
     f <- capture histogram 
     let v1 = Vector (V.fromList [0,0,0,4,4:: USize]) (One 5)
         local = Vector (V.fromList (replicate 256 (1 :: Int32))) (One 256)
     
     (Vector dat n) <- execute f (local :- v1)
     liftIO$ putStrLn$ show dat
                    
    