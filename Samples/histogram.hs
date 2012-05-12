{-# LANGUAGE ScopedTypeVariables #-} 

{- 2012 Joel Svensson -} 

module Test where 

import Intel.ArBB 

import qualified Data.Vector.Storable as V 

import Data.Int
import Data.Word


-- TODO: The c++ version uses some kind of trick where a 1 is turned into an array of suitable size 
--       add_merge(1,input,256) ... 


histogram :: Exp (Vector Int32)  -> Exp (Vector Int32)
histogram input = addMerge local (toUSize input) 256 
    where
      local = constVector 1 256
 

testHist = 
  withArBB $ 
  do 
     f <- capture histogram 
     let v1 = Vector (V.fromList [0,0,0,4,4:: Int32]) (One 5)
       
     
     (Vector dat n) <- execute f  v1
     liftIO$ putStrLn$ show dat
                    
    