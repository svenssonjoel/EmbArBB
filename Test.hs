{-# LANGUAGE ScopedTypeVariables #-} 

{- 2012 Joel Svensson -} 

module Test where 

import Intel.ArBB 
import qualified Intel.ArbbVM as ArBB 
import qualified Intel.ArbbVM.Convenience as ArBB 
import Intel.ArbbVM.Convenience (liftIO)

import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr

import Data.Int 
import Data.Word
import qualified Data.Map as Map

import qualified Data.Map as Map 


---------------------------------------------------------------------------- 
--  Testing Testing 

t1 :: Exp (Vector Int32) ->  Exp (Scalar Int32) -- Exp (Vector0D Int32)
t1 input = addReduce input -- E $ LAddReduce (newLabel ()) input 

t2 :: Exp (Scalar Int32) -> Exp (Scalar Int32) 
t2 input = input + input -- (E input) = E $ LBinOp (newLabel ()) Add input input

t3 input = t2 (t1 input)

t4 input = t2 (t2 (t3 (input)))


-- TODO: Try to get the crossProd3D function to go through ArBB generation. 
-- TODO: Need to overload *,+,- on Vectors also .. 
 
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
 

{- TODO: Types are needed to generate the ArBB code. 
   TODO: Make the cheat generate runnable code for t1 and t3 
   TODO: ... 
-} 
genArBBFunCheat :: DAG -> NodeID -> ArBB.EmitArbb ArBB.ConvFunction
genArBBFunCheat dag nod = 
  do 
    st <- ArBB.getScalarType_ ArBB.ArbbI32 -- t3 returns a single scalar 
    dt <- ArBB.getDenseType_ st 1 
    
    let rt = st 
        it = dt 

    ArBB.funDef_ "Cheat" [rt] [it] $ \ [out] inp -> do 
      v <- genBodyArBB dag nod inp 
      -- ArBB.op_ ArBB.ArbbOpCopy [out] [v]
      ArBB.copy_ out v


      
--                                  inputs 
genBodyArBB :: DAG -> NodeID -> [ArBB.Variable] -> ArBB.EmitArbb ArBB.Variable 
genBodyArBB dag nod inputs =                           
  do 
    case Map.lookup nod dag of 
      Nothing -> error "genBodyArBB: broken" 
      (Just node) -> genNode node 
  where 
    genNode (NReduce Add nid) = 
      do
        v1 <- genBodyArBB dag nid inputs 
        -- v@(ArBBArray _ _ v' ) <- newArBBArray Zero (ArBB.ArbbI32)
        st <- ArBB.getScalarType_ ArBB.ArbbI32
--        dt <- ArBB.getDenseType_ st 1 
        imm <- ArBB.createLocal_ st "res" 
        ArBB.opDynamic_ ArBB.ArbbOpAddReduce [imm] [v1] 
        
        
        liftIO$ putStrLn "NReduce Add node" 
        return imm
    genNode (NBinOp Add n1 n2) = 
      do 
        -- If i just do this, will it unshare the computation ? 
        -- Yes, it does, but it doesnt matter this is just a test...  
        v1 <- genBodyArBB dag n1 inputs
        v2 <- genBodyArBB dag n2 inputs 
        st <- ArBB.getScalarType_ ArBB.ArbbI32 -- CHEAT 
    
        imm <- ArBB.createLocal_ st "res2" 
        ArBB.op_ ArBB.ArbbOpAdd [imm] [v1,v2] 
        
        return imm
        
    genNode (NVar (Variable nom)) = return$ head inputs -- only one input in this cheat. 
    --genNode whatever = liftIO$ putStrLn$ "hello" -- show whatever
    

        
-- Test t1 
    
test_t1 = 
  ArBB.arbbSession$ 
   ArBB.withArray_ [0..9 :: Int32] $ \ inp -> 
    do 
      -- (ArBBArray _ _ v) <- uploadArrayVector v1 
      st <- ArBB.getScalarType_ ArBB.ArbbI32 
      dt <- ArBB.getDenseType_ st 1 

      inb <- ArBB.createDenseBinding_ (castPtr inp) 1 [10] [4]
      gin <- ArBB.createGlobal_ dt "input" inb
      vin <- ArBB.variableFromGlobal_ gin
      
      g  <- ArBB.createGlobal_nobind_ st "res" --st "res" 
      y  <- ArBB.variableFromGlobal_ g
        
      let (start_node,c) = compile t4
      fun <- genArBBFunCheat c start_node

      str <- ArBB.serializeFunction_ fun
      liftIO$ putStrLn (ArBB.getCString str)
      
      liftIO$ putStrLn$ show c
      liftIO$ putStrLn$ show $ typecheckDAG c (Map.fromList [(Variable "Input",Dense I (ArBB.ArbbI32))])
      
      ArBB.execute_ fun [y] [vin] 
      
      --result <- readBackVector (ArBBArray (One 10) ArBB.ArbbI32 y)
      result :: Int32 <- ArBB.readScalar_ y  
    
      liftIO$ putStrLn $ show result
