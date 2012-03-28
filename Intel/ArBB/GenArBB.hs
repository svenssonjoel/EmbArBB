{- 2012 Joel Svensson -} 


module Intel.ArBB.GenArBB where 

import Intel.ArBB.DAG
import Intel.ArBB.Syntax 
import Intel.ArBB.TypeCheck 
import Intel.ArBB.Types
import Intel.ArBB.Data.Int 


import qualified Intel.ArbbVM as VM 
import qualified Intel.ArbbVM.Convenience as VM

import qualified Data.Map as Map
import qualified Data.List as L
import Data.Maybe 

import Control.Monad.State
---------------------------------------------------------------------------- 
-- 
type Gen a = StateT (Map.Map NodeID [VM.Variable]) VM.EmitArbb a


----------------------------------------------------------------------------
-- Wrapper 
genBody :: DAG 
           -> NodeID 
           -> NodeIDType 
           -> (Map.Map FunctionName VM.ConvFunction) 
           -> [VM.Variable] 
           -> VM.EmitArbb [VM.Variable] 
genBody dag nid typem funm is = evalStateT (genBody' dag nid typem funm is) (Map.empty) 

accmBody :: DAG
            -> [NodeID] 
            -> NodeIDType 
            -> (Map.Map FunctionName VM.ConvFunction)
            -> [VM.Variable] 
            -> VM.EmitArbb [VM.Variable]
accmBody dag nids typem funm is = liftM fst $ doBody nids (Map.empty) 

  where 
    doBody ::[NodeID] -> (Map.Map NodeID [VM.Variable]) -> VM.EmitArbb ([VM.Variable],(Map.Map NodeID [VM.Variable]))
    doBody [] m = return ([],m) 
    doBody (x:xs) m =
      do 
          (vs,m') <- runStateT (genBody' dag x typem funm is) m
          (vss,m'') <- doBody xs m'
          return (vs++vss,m'')

----------------------------------------------------------------------------
-- Helpers 
visited :: NodeID -> Gen (Maybe [VM.Variable]) 
visited nid = 
  do 
    m <- get 
    return$ Map.lookup nid m 

addNode :: NodeID -> [VM.Variable] -> Gen () 
addNode nid v = 
  do 
    m <- get 
    let m' = Map.insert nid v m 
    put m' 
    


getTypeOfNode :: NodeID -> NodeIDType -> Gen [VM.Type] 
getTypeOfNode n m = 
  let t = fromJust$ Map.lookup n m 
  in 
    lift$ toArBBType t 


----------------------------------------------------------------------------
-- The real worker
genBody' :: DAG 
           -> NodeID 
           -> NodeIDType 
           -> (Map.Map FunctionName VM.ConvFunction) 
           -> [VM.Variable] 
           -> Gen [VM.Variable] 
genBody' dag nid typem funm is = 
  do 
    m <- get 
    case Map.lookup nid m of 
      (Just v) -> return v 
      Nothing   -> 
          case Map.lookup nid dag of 
            (Just node) -> genNode nid node 
            Nothing -> error "genBody: DAG is broken" 
            
  where 
    genNode :: NodeID -> Node -> Gen [VM.Variable]
    genNode thisNid (NLit l) = 
      do 
        lift$ VM.liftIO$ putStrLn$  show l
        v <- genLiteral l 
        return [v] 
    genNode thisNid (NVar (Variable nom)) = 
      do 
        lift$ VM.liftIO$ putStrLn "Var node" 
        return$ [is !! (read (nom L.\\ "v") :: Int)]  -- inputs
    
    genNode thisNid (NReduce op n1 n2) = 
      do
        
        [t] <- getTypeOfNode thisNid typem
        v1 <- genBody' dag n1 typem funm is 
        v2 <- genBody' dag n2 typem funm is 
        
        imm <- lift$ VM.createLocal_ t "imm"   -- st "res" 
        lift$ VM.opDynamic_ (opToArBBReduceOp op) [imm] (v1 ++ v2) 
        
        -- memoize the computed var
        addNode thisNid [imm] 
        
        lift$ VM.liftIO$ putStrLn "NReduce node" 
        return [imm]
    genNode thisNid (NScan op n1 n2 n3) = 
      do
        
        [t] <- getTypeOfNode thisNid typem
        v1 <- genBody' dag n1 typem funm is 
        v2 <- genBody' dag n2 typem funm is 
        v3 <- genBody' dag n3 typem funm is 
        
        imm <- lift$ VM.createLocal_ t "imm"   -- st "res" 
        lift$ VM.op_ (opToArBBScanOp op) [imm] (v1 ++ v2 ++ v3) 
        
        -- memoize the computed var
        addNode thisNid [imm] 
        
        lift$ VM.liftIO$ putStrLn "NScan  node" 
        return [imm]
    genNode thisNid (NBinOp op n1 n2) = 
      do 
        v1 <- genBody' dag n1 typem funm is 
        v2 <- genBody' dag n2 typem funm is 
        
        [t] <- getTypeOfNode thisNid typem
        
        imm <- lift $ VM.createLocal_ t "imm" 
        -- Assume v1,v2 list of length one 
        lift$ VM.op_ (opToArBB op) [imm] (v1 ++ v2) 
        
        -- memoize the computed var
        addNode thisNid [imm] 
        
        lift$ VM.liftIO$ putStrLn "BinOp node" 
        return [imm]
    genNode thisNid (NUnOp op n1) = 
      do 
        v1 <- genBody' dag n1 typem funm is 
        
        [t] <- getTypeOfNode thisNid typem
        
        imm <- lift $ VM.createLocal_ t "imm" 
        lift$ VM.op_ (opToArBB op) [imm] v1 
        
        -- memoize the computed var
        addNode thisNid [imm] 
        
        lift$ VM.liftIO$ putStrLn "UnOp node" 
        return [imm]
    genNode thisNid (NRotate n1 n2) = 
      do 
        v1 <- genBody' dag n1 typem funm is 
        v2 <- genBody' dag n2 typem funm is 
        
        [t] <- getTypeOfNode thisNid typem 
        
        imm <- lift$ VM.createLocal_ t "imm" 
        lift$ VM.opDynamic_ VM.ArbbOpRotate [imm] (v1 ++ v2)
        
        addNode thisNid [imm] 
        lift$ VM.liftIO$ putStrLn "Rotate node" 
        return [imm]
    genNode thisNid (NRotateRev n1 n2) = 
      do 
        v1 <- genBody' dag n1 typem funm is 
        v2 <- genBody' dag n2 typem funm is 
        
        [t] <- getTypeOfNode thisNid typem 
        
        imm <- lift$ VM.createLocal_ t "imm" 
        lift$ VM.opDynamic_ VM.ArbbOpRotateReverse [imm] (v1 ++ v2)
        
        addNode thisNid [imm] 
        lift$ VM.liftIO$ putStrLn "RotateRev node" 
        return [imm]
    genNode thisNid (NSortRank n1 n2) = 
      do 
        
        v1 <- genBody' dag n1 typem funm is 
        v2 <- genBody' dag n2 typem funm is 
        
        [t1,t2] <- getTypeOfNode thisNid typem 
       
        imm <- lift$ VM.createLocal_ t1 "imm" 
        ranks <- lift$ VM.createLocal_ t2 "imm"
      
        lift$ VM.op_ VM.ArbbOpSortRank [imm,ranks] (v1 ++ v2)
        
        addNode thisNid [imm,ranks] 
        lift$ VM.liftIO$ putStrLn "SortRank node" 
        return [imm,ranks]
    genNode thisNid (NResIndex n i) = 
      do 
        vs <- genBody' dag n typem funm is 
        return$ [vs !! i]
    genNode thisNid (NIndex0 n) = 
      do 
        vs <- genBody' dag n typem funm is 
        return$ vs
     
    
        
        

        
genLiteral :: Literal -> Gen VM.Variable
genLiteral (LitInt8 i)  = lift$ VM.int8_ i 
genLiteral (LitInt16 i) = lift$ VM.int16_ i 
genLiteral (LitInt32 i) = lift$ VM.int32_ i 
genLiteral (LitWord32 i) = lift$ VM.uint32_ i 
genLiteral (LitFloat i) = lift$ VM.float32_ i 
genLiteral (LitDouble i) = lift$ VM.float64_ i 
genLiteral (LitISize (ISize i)) = lift$ VM.isize_ i
genLiteral (LitUSize (USize i)) = lift$ VM.usize_ i



opToArBB Add = VM.ArbbOpAdd
opToArBB Mul = VM.ArbbOpMul 
opToArBB Sub = VM.ArbbOpSub
opToArBB Min = VM.ArbbOpMin
opToArBB Max = VM.ArbbOpMax
-- opToArBB And = VM.ArbbOpAnd
-- opToArBB Ior = VM.ArbbOpIor
-- opToArBB Xor = VM.ArbbOpXor
-- opToArBB Abs = VM.ArbbOpAbs
opToArBB Acos = VM.ArbbOpAcos
opToArBB Asin = VM.ArbbOpAsin
opToArBB Atan = VM.ArbbOpAtan
opToArBB Ceil = VM.ArbbOpCeil
opToArBB Cos = VM.ArbbOpCos
opToArBB Cosh = VM.ArbbOpCosh
opToArBB Exp = VM.ArbbOpExp
opToArBB Exp10 = VM.ArbbOpExp10
opToArBB Floor = VM.ArbbOpFloor
opToArBB Ln  = VM.ArbbOpLn
opToArBB Log10 = VM.ArbbOpLog10
opToArBB Log_not = VM.ArbbOpLogNot
opToArBB Bit_not = VM.ArbbOpBitNot
opToArBB Rcp = VM.ArbbOpRcp
opToArBB Round = VM.ArbbOpRound
opToArBB Rsqrt = VM.ArbbOpRsqrt
opToArBB Sin  = VM.ArbbOpSin
opToArBB Sinh = VM.ArbbOpSinh
opToArBB Sqrt = VM.ArbbOpSqrt
opToArBB Tan = VM.ArbbOpTan
opToArBB Tanh = VM.ArbbOpTanh
opToArBB Neg = VM.ArbbOpNeg
opToArBB Bit_and  = VM.ArbbOpBitAnd
opToArBB Atan2 = VM.ArbbOpAtan2
opToArBB Compare = VM.ArbbOpCompare
opToArBB Equal = VM.ArbbOpEqual
opToArBB Geq = VM.ArbbOpGeq
opToArBB Bit_or = VM.ArbbOpBitOr
opToArBB Leq = VM.ArbbOpLeq
opToArBB Less = VM.ArbbOpLess
opToArBB Log_and = VM.ArbbOpLogAnd
opToArBB Log_or = VM.ArbbOpLogOr
opToArBB Lsh = VM.ArbbOpLsh
opToArBB Mod = VM.ArbbOpMod
opToArBB Neq = VM.ArbbOpNeq
opToArBB Pow = VM.ArbbOpPow
opToArBB Rsh = VM.ArbbOpRsh
opToArBB Bit_xor = VM.ArbbOpBitXor
opToArBB Select = VM.ArbbOpSelect





-- TODO: go on

opToArBBReduceOp Add = VM.ArbbOpAddReduce
opToArBBReduceOp Mul = VM.ArbbOpMulReduce
opToArBBReduceOp Max = VM.ArbbOpMaxReduce
opToArBBReduceOp Min = VM.ArbbOpMinReduce
opToArBBReduceOp And = VM.ArbbOpMinReduce
opToArBBReduceOp Ior = VM.ArbbOpIorReduce
opToArBBReduceOp Xor = VM.ArbbOpXorReduce


opToArBBScanOp Add = VM.ArbbOpAddScan
opToArBBScanOp Mul = VM.ArbbOpMulScan
opToArBBScanOp Max = VM.ArbbOpMaxScan
opToArBBScanOp Min = VM.ArbbOpMinScan
opToArBBScanOp And = VM.ArbbOpMinScan
opToArBBScanOp Ior = VM.ArbbOpIorScan
opToArBBScanOp Xor = VM.ArbbOpXorScan



----------------------------------------------------------------------------   
-- 
        
toArBBType :: Type -> VM.EmitArbb [VM.Type]
toArBBType (Scalar t) = 
  do 
   
    t' <- VM.getScalarType_ t
    return [t'] 
toArBBType (Dense I t) = 
  do 
   
    st <- VM.getScalarType_ t
    t' <- VM.getDenseType_ st 1 
    return [t']
toArBBType (Dense II t) = 
  do 
   
    st <- VM.getScalarType_ t
    t' <- VM.getDenseType_ st 2 
    return [t']
toArBBType (Dense III t) = 
  do 
   
    st <- VM.getScalarType_ t
    t' <- VM.getDenseType_ st 3
    return [t'] 
toArBBType (Tuple []) = return [] 
toArBBType (Tuple (t:ts)) = 
  do
   
    ts' <- toArBBType (Tuple ts) 
    -- Tuples will not contain tuples ! 
    [t']  <- toArBBType t
    return $ t':ts'



    
    
