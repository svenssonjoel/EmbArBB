{-# LANGUAGE ScopedTypeVariables #-} 

{- 2012 Joel Svensson -} 

module Test where 

import Intel.ArBB 
import qualified Intel.ArbbVM as VM
import qualified Intel.ArbbVM.Convenience as VM
-- import Intel.ArbbVM.Convenience (liftIO)

import Intel.ArBB.WithArBB
import Intel.ArBB.Vector

import qualified Data.Vector.Storable as V 

import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr

import Data.Int 
import Data.Word
import qualified Data.Map as Map
import Control.Monad.State hiding (liftIO)


---------------------------------------------------------------------------- 
--  Testing Testing 

t1 :: Exp (Vector Int32) ->  Exp (Scalar Int32) -- Exp (Vector0D Int32)
t1 input = addReduce input 

t2 :: Exp (Scalar Int32) -> Exp (Scalar Int32) 
t2 input = input + input 

t3 input = t2 (t1 input)

t4 input = t2 (t2 (t3 (input)))

----------------------------------------------------------------------------
-- 3d vector crossProduct 
crossProd3D :: Exp (Vector Float) -> Exp (Vector Float) -> Exp (Vector Float) 
crossProd3D v1 v2 = lprods - rprods 
  where 
    v1' = rotate v1 1 
    v2' = rotateRev v2 1
    v1'' = rotate v1' 1 
    v2'' = rotateRev v2' 1 
    lprods = v1' * v2' 
    rprods = v1'' * v2'' 
    
    
-- Experiment: call a named function 
callCP3D :: Exp (Vector Float) -> Exp (Vector Float) -> Exp (Vector Float) 
callCP3D v1 v2 = resIndex (call (Function "crossProd") (v1 :- v2)) 0
 

----------------------------------------------------------------------------
-- Small tests 
test1 = withArBB $ capture t1
test2 = withArBB $ capture t2 
test3 = withArBB $ capture t3

---------------------------------------------------------------------------- 
test4 = 
  withArBB $ 
   do
     
    let v1 = Vector (V.fromList [0..9 :: Int32]) (One 10)
     
    f <- capture t4 
    
    (Vector r dim) <- execute f v1
    
    liftIO$ putStrLn$ show r
 


---------------------------------------------------------------------------
-- getting serious 
test5 = 
  -- Run an ArBB session
  withArBB $  
  do
    -- turn an embedded language function into an ArBB function.
    -- JIT takes place here
    f <- capture crossProd3D 
    
    -- create two one dimensional input Vectors of length 3 
    let v1 = Vector (V.fromList [1.5,1.4,0.2]) (One 3)
        v2 = Vector (V.fromList [1.5,1.8,1.9]) (One 3)
    
    -- execute f with inputs v1 and v2 
    (Vector dat n) <- execute f (v1 :- v2) 
    liftIO$ putStrLn$ show dat
    
    -- f can be used again and again (jit only once)
    execute f (v2 :- v1) 
