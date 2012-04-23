{-# LANGUAGE ScopedTypeVariables #-} 

{- 2012 Joel Svensson -} 

module Test where 

import Intel.ArBB 
import qualified Intel.ArbbVM as VM
import qualified Intel.ArbbVM.Convenience as VM

import Intel.ArBB.WithArBB
import Intel.ArBB.Vector
import Intel.ArBB.Data.Int

import qualified Data.Vector.Storable as V 

import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr

import Data.Int 
import Data.Word
import Data.IORef
import qualified Data.Map as Map
import Control.Monad.State hiding (liftIO)


---------------------------------------------------------------------------- 
--  Testing Testing 

t1 :: Exp (Vector Int32) ->  Exp (Scalar Int32)
t1 input = addReduce0 input 

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
    
dotProd :: Exp (Vector Float) -> Exp (Vector Float) -> Exp Float
dotProd v1 v2 = index0$ addReduce0 (v1 * v2)
    
-- Experiment: call a named function 
--callCP3D :: Exp (Vector Float) -> Exp (Vector Float) -> Exp (Vector Float) 
--callCP3D v1 v2 = resIndex (call (Function "crossProd") (v1 :- v2)) 0
 
---------------------------------------------------------------------------- 
-- getRanks 
getRanks :: Exp (Vector Float) -> Exp (Vector USize) 
getRanks v1 = snd (sortRank v1 0) 

getBoth :: Exp (Vector Float) -> (Exp (Vector Float), Exp (Vector USize)) 
getBoth v1 = sortRank v1 0 
  
getBoth' :: Exp (Vector Float) -> (Exp (Vector Float, Vector USize)) 
getBoth' v1 = sortRank' v1 0 


----------------------------------------------------------------------------
-- test reduce 2D to 1D 

red :: Exp (DVector Dim2 Int32) -> Exp (Vector Int32) 
red v1 = addReduce0 v1
 
sca :: Exp (DVector Dim2 Int32) -> Exp (DVector Dim2 Int32) 
sca v1 = addScan v1 0 0

----------------------------------------------------------------------------
cond :: () -> (Exp Int32) 
cond () = ifThenElse (E $ LLit (newLabel ()) $ LitBool True) 1  2 



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


test6 = 
  withArBB $  
  do
    f <- capture getRanks
    
    -- create input
    let v1 = Vector (V.fromList [4,3,2,1::Float]) (One 4)
    
    --str <- serialize f 
    --liftIO$ putStrLn str
    
    -- execute f 
    (Vector dat n) <- execute f v1
    liftIO$ putStrLn$ show dat
    
    

test6' = 
  withArBB $  
  do
    f <- capture getBoth
    
    -- create input
    let v1 = Vector (V.fromList [4,3,2,1::Float]) (One 4)

    str <- serialize f 
    liftIO$ putStrLn str
    
    --  execute f 
    ((Vector dat n) :- Vector ranks n2) <- execute f v1
    liftIO$ putStrLn$ show dat  ++ " " ++ show ranks   
 
test6'' = 
  withArBB $  
  do
    f <- capture getBoth'
    
    -- create input
    let v1 = Vector (V.fromList [4,3,2,1::Float]) (One 4)

    str <- serialize f 
    liftIO$ putStrLn str
    
    -- execute f 
    (Vector dat n ,Vector ranks n2) <- execute f v1
    liftIO$ putStrLn$ show dat  ++ " " ++ show ranks   

   
test7 = 
  withArBB $  
  do
    f <- capture dotProd
    
    -- create input
    let v1 = Vector (V.fromList [1,2,3::Float]) (One 3)
        v2 = Vector (V.fromList [3,2,1::Float]) (One 3)
    str <- serialize f 
    liftIO$ putStrLn str
    
    -- execute f 
    dat <- execute f (v1 :- v2)
    liftIO$ putStrLn$ show dat



----------------------------------------------------------------------------
    
testRotate = 
  withArBB $  
  do
    f <- capture rot
    g <- capture rotRev
    
    -- create input
    let v1 = Vector (V.fromList [1,2,3::Float]) (One 3)
        
    str <- serialize f 
    liftIO$ putStrLn str
    
    str <- serialize g 
    liftIO$ putStrLn str
    
    -- execute f 
    (Vector dat n) <- execute f v1
    liftIO$ putStrLn$ "Rotate " ++ show dat
    
    -- execute g 
    (Vector dat n) <- execute g v1
    liftIO$ putStrLn$ "RotateRev " ++ show dat
    
  where 
    rot :: Exp (Vector Float) -> Exp (Vector Float) 
    rot v = rotate v 1 
    
    rotRev :: Exp (Vector Float) -> Exp (Vector Float)
    rotRev v = rotateRev v 1 
    



----------------------------------------------------------------------------
testScan = 
  withArBB $  
  do
    f <- capture s1
    g <- capture s2
    h <- capture s3
    
    -- create input
    let v1 = Vector (V.fromList [1..10::Float]) (One 10)
        
    str <- serialize f 
    liftIO$ putStrLn str
    
    str <- serialize g 
    liftIO$ putStrLn str
    
    -- execute f 
    (Vector dat n) <- execute f v1
    liftIO$ putStrLn$ "s1 " ++ show dat
    
    -- execute g 
    (Vector dat n) <- execute g v1
    liftIO$ putStrLn$ "s2 " ++ show dat
    
    --execute h
    (Vector dat n) <- execute h v1
    liftIO$ putStrLn$ "s3 " ++ show dat
    
    
    
  where 
    s1 :: Exp (Vector Float) -> Exp (Vector Float) 
    s1 v = addScan v 0 0
    
    s2 :: Exp (Vector Float) -> Exp (Vector Float)
    s2 v = addScan v 1 0 
    
    s3 :: Exp (Vector Float) -> Exp (Vector Float)
    s3 v = mulScan v 1 0 
    
    
testReduce = 
  withArBB $  
  do
    f <- capture red
 
    -- create input
    let v1 = Vector (V.fromList [0..9::Int32]) (Two 5 2)
        
    str <- serialize f 
    liftIO$ putStrLn str
    -- execute f 
    (Vector dat n) <- execute f v1
    liftIO$ putStrLn$ show dat
    
testSca = 
  withArBB $  
  do
    f <- capture sca
 
    -- create input
    let v1 = Vector (V.fromList [0..9::Int32]) (Two 5 2)
        
    str <- serialize f 
    liftIO$ putStrLn str
    -- execute f 
    (Vector dat n) <- execute f v1
    liftIO$ putStrLn$ show dat



testCond = 
  withArBB $  
  do
    f <- capture cond
    return f
    str <- serialize f 
    liftIO$ putStrLn str
    -- execute f 
    dat <- execute f ()
    liftIO$ putStrLn$ show dat


testIndex = 
  withArBB $  
  do
    f <- capture fun
    return f
    str <- serialize f 
    liftIO$ putStrLn str
    
    let v1 = Vector (V.fromList [0..9::Int32]) (One 10)
    
    -- execute f 
    dat <- execute f v1
    liftIO$ putStrLn$ show dat
  where 
    fun :: Exp (DVector Dim1 Int32) -> Exp Int32
    fun v = index1 v 3
   
testScalar = 
  withArBB $  
  do
    f <- capture fun
    return f
    str <- serialize f 
    liftIO$ putStrLn str
  
    -- execute f 
    dat <- execute f 10
    liftIO$ putStrLn$ show dat
  where 
    fun :: Exp Int32 -> Exp Int32
    fun v = v+v
  

-- DONE: shows a bug. The two scalar outputs are messed up. 
--       (SIMPLE FIX: unrelated to the bug that appears in the loop example)  
testScalar2 = 
  withArBB $  
  do
    f <- capture fun
    return f
    str <- serialize f 
    liftIO$ putStrLn str
    
    let v1 = Vector (V.fromList [0..9::Int32]) (One 10)
    
    -- execute f 
    (Vector vout n :- dat :- r) <- execute f (v1 :- 10)
    liftIO$ putStrLn$ show vout
    liftIO$ putStrLn$ show dat
    liftIO$ putStrLn$ show r
    
    
  where 
    fun :: Exp (Vector Int32) -> Exp Int32 -> (Exp (Vector Int32), Exp Int32, Exp Int32)
    fun v i = (v'+v',k,i+j)
      where v' = v + v 
            j  = index1 v' 3
            k  = index0 (addReduce0 v)

-- TODO: Works if using -larbb_dev and ARBB_OPT_LEVEL=O2.
--       Breaks on all other settings.. 
testWhile = 
  withArBB $  
  do
    f <- capture fun
   
    str <- serialize f
    liftIO$ putStrLn str
  
  
    let v1 = Vector (V.fromList [0..9999::Int32]) (One 10000)
    
    -- execute f 
    -- (Vector dat n,i) <- execute f v1
    i <- execute f v1
    -- liftIO$ putStrLn$ show dat
    liftIO$ putStrLn$ show i
    

  where
    fun :: Exp (DVector Dim1 Int32) -> ({-Exp (DVector Dim1 Int32),-}Exp Int32)
    fun v = 
      let (x,y) = while (\(v',i)  -> i <* 10)  
                        (\(v',i) -> (v + v',i+1))
                        (v,0)
      in (y + index0 (addReduce0 x))
                            
testWhile2 = 
  withArBB $  
  do
    f <- capture fun
   
    str <- serialize f
    liftIO$ putStrLn str
  
    
    -- execute f 
    (dat :- i) <- execute f 1
    liftIO$ putStrLn$ show dat
    liftIO$ putStrLn$ show i
    

  where
    fun :: Exp Int32 -> (Exp Int32,Exp Int32)
    fun v = 
      let (x,y) = while (\(v',i) -> i <* 10)  
                        (\(v',i) -> (v + v',i+1))
                        (v,0)
      in (x+1,y+1)





----------------------------------------------------------------------------
testAPA= 
  withArBB $  
  do
    c <- capture2 fun
    
    a <- liftIO$ newIORef (0 :: Int32) 
    
    execute2 c 10 a 
    
    a' <- liftIO$ readIORef a 
    
    liftIO$ putStrLn $ show a' 
    
  where 
    fun :: Exp Int32 -> Exp Int32
    fun i = i+i+5
