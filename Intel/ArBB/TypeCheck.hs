{- 2012 Joel Svensson -}

module Intel.ArBB.TypeCheck where 


{- Typecheck a DAG. and create a nodeID_to_type map -} 

import Intel.ArBB.Types 

import Intel.ArBB.DAG
import Intel.ArBB.Syntax

import qualified Intel.ArbbVM as ArBB

import Control.Monad.State 
import Control.Monad.Identity
import qualified Data.Map as Map 



---------------------------------------------------------------------------- 
-- Typechecking state
type VarType      = Map.Map Variable Type 

type NodeIDType   = Map.Map NodeID Type 

type CheckState' m a = StateT (VarType,NodeIDType) m a 
type TypeChecker m a = StateT (VarType,NodeIDType) m a

runTypeChecker :: Monad m => TypeChecker m a -> VarType -> m a 
runTypeChecker c vt = evalStateT c (vt,Map.empty)

runTypeChecker' :: Monad m => TypeChecker m a -> (VarType,NodeIDType) -> m (a,(VarType,NodeIDType))
runTypeChecker' c vt = runStateT c vt 

type CheckState a = CheckState' Identity a 

varType :: Monad m => Variable -> TypeChecker m (Maybe Type)
varType v = 
  do 
    varsTypes <- liftM fst $ get 
    return $ Map.lookup v varsTypes 
  
addNodeIDType :: Monad  m => NodeID -> Type -> TypeChecker m () 
addNodeIDType n t = 
  do 
    (v,nodts) <- get
    let nodts' = Map.insert n t nodts 
    put (v,nodts')

----------------------------------------------------------------------------
-- Knowing the types at every node is required to generate the ArBB code. 

-- TODO: MUCH Cheatinh going on here. 
--       But on the other hand, the phantom type system that wraps the "Language" 
--       should provide some guarantees. (Maybe can be quite relaxed here) 
typecheckNID :: Monad m => DAG -> NodeID -> TypeChecker m Type
typecheckNID d n = 
  do 
    let node = case Map.lookup n d of 
                  Nothing -> error "typecheckNID: DAG is broken" 
                  (Just nod) -> nod
    typ  <- typecheckNode d node 
    case typ of 
      Nothing -> error $ "typecheckNID: typecheck error " ++ show (Map.lookup n d) 
      (Just typ') -> 
        do 
          addNodeIDType n typ'
          return typ' 
      
    
  where 
    typecheckNode dag (NLit l) = typecheckLiteral l
    typecheckNode dag (NVar v) = varType v 
    typecheckNode dag (NIndex0 n) = 
      do 
        nt <- typecheckNID dag n 
        if isScalar nt 
          then return$ Just$ nt 
          else return Nothing 

    typecheckNode dag (NResIndex n i) =   
      do 
        (Tuple nt) <- typecheckNID dag n 
        return$ Just$ nt !! i 
    typecheckNode dag (NIf n1 n2 n3) = 
      do 
        -- TODO: FIX 
        t1 <- typecheckNID dag n1
        t2 <- typecheckNID dag n2 
        t3 <- typecheckNID dag n3 
        return $ Just t2
    typecheckNode dag (NOp op ns) = 
      do
        ts <- mapM (typecheckNID dag) ns 
        return$ typeOfOp op ts 
    typecheckNode dag (NWhile vars cond body st) = 
      do 
        typecheckNID dag cond 
        mapM (typecheckNID dag) body
        ts <- mapM (typecheckNID dag) st 
        return$ Just$ Tuple ts -- TODO: Structured result ?
      
    typecheckLiteral (LitInt8 _)   = return$ Just$ Scalar ArBB.ArbbI8
    typecheckLiteral (LitInt16 _)  = return$ Just$ Scalar ArBB.ArbbI16
    typecheckLiteral (LitInt32 _)  = return$ Just$ Scalar ArBB.ArbbI32
    typecheckLiteral (LitWord32 _) = return$ Just$ Scalar ArBB.ArbbU32
    typecheckLiteral (LitISize _)  = return$ Just$ Scalar ArBB.ArbbIsize 
    typecheckLiteral (LitUSize _)  = return$ Just$ Scalar ArBB.ArbbUsize 
    typecheckLiteral (LitBool _)   = return$ Just$ Scalar ArBB.ArbbBoolean
    
    same t1 t2 = if t1 == t2 then Just t1 else Nothing 
    
    -- LOTS of cheating going on 
    -- TODO: replace all "undefined" with something useful. 
    typeOfOp :: Op -> [Type] -> Maybe Type
    typeOfOp Add [t1,t2] = same t1 t2
    typeOfOp Mul [t1,t2] = same t1 t2
    typeOfOp Sub [t1,t2] = same t1 t2
    typeOfOp Min [t1,t2] = same t1 t2
    typeOfOp Max [t1,t2] = same t1 t2
    typeOfOp Acos [t] = Just t 
    typeOfOp Asin [t] = Just t 
    typeOfOp Atan [t] = Just t 
    typeOfOp Ceil [t] = Just t 
    typeOfOp Cos  [t] = Just t 
    typeOfOp Cosh [t] = Just t 
    typeOfOp Exp  [t] = Just t 
    typeOfOp Exp10 [t] = Just t 
    typeOfOp Floor [t] = Just t 
    typeOfOp Ln [t] = Just t    
    typeOfOp Log10 [t] = Just t  
    typeOfOp Log_not [t] = Just t  -- Bool -> Bool
    typeOfOp Bit_not [t] = Just t  -- Bits a => a -> a 
    typeOfOp Rcp xs = undefined 
    typeOfOp Round xs = undefined 
    typeOfOp Rsqrt xs = undefined 
    typeOfOp Sin  xs = undefined 
    typeOfOp Sinh xs = undefined 
    typeOfOp Sqrt xs = undefined 
    typeOfOp Tan xs = undefined 
    typeOfOp Tanh xs = undefined 
    typeOfOp Neg xs = undefined 
    typeOfOp Bit_and  xs = undefined 
    typeOfOp Atan2 xs = undefined 
    typeOfOp Compare xs = undefined 
    typeOfOp Equal xs = undefined 
    typeOfOp Geq xs = undefined 
    typeOfOp Bit_or xs = undefined 
    typeOfOp Leq xs = undefined 
    typeOfOp Less [a,b] = Just $ Scalar ArBB.ArbbBoolean
    typeOfOp Log_and xs = undefined 
    typeOfOp Log_or xs = undefined 
    typeOfOp Lsh xs = undefined 
    typeOfOp Mod xs = undefined 
    typeOfOp Neq xs = undefined 
    typeOfOp Pow xs = undefined 
    typeOfOp Rsh xs = undefined 
    typeOfOp Bit_xor xs = undefined 
    typeOfOp Select xs = undefined 
    typeOfOp Gather xs = undefined 
    typeOfOp Scatter xs = undefined 
    typeOfOp Pack xs = undefined 
    typeOfOp Unpack xs = undefined 
    typeOfOp Shuffle xs = undefined 
    typeOfOp Unshuffle xs = undefined 
    typeOfOp Repeat xs = undefined 
    typeOfOp Distribute xs = undefined 
    typeOfOp RepeatRow xs = undefined 
    typeOfOp RepeatCol xs = undefined 
    typeOfOp RepeatPage xs = undefined 
    typeOfOp Transpose xs = undefined 
    typeOfOp SwapCol xs = undefined 
    typeOfOp SwapRow xs = undefined 
    typeOfOp SwapPage xs = undefined 
    typeOfOp ShiftConst xs = undefined 
    typeOfOp ShiftClamp xs = undefined 
    typeOfOp ShiftConstRev xs = undefined 
    typeOfOp ShiftClampRev xs = undefined 
    typeOfOp Rotate [v,d] = Just v 
    typeOfOp RotateRev [v,d] = Just v
    typeOfOp Reverse [v] = Just v 
    typeOfOp Length xs = undefined 
    typeOfOp ApplyNesting xs = undefined 
    typeOfOp GetNesting xs = undefined 
    typeOfOp Cat xs = undefined 
    typeOfOp Cast xs = undefined 
    
    typeOfOp Extract [Dense I a,i] = Just $ Scalar a 
    typeOfOp Extract [Dense II a,i,j] = Just $ Scalar a 
    typeOfOp Extract [Dense III a,i,j,k] = Just $ Scalar a 
    
    typeOfOp Split xs = undefined 
    typeOfOp Unsplit xs = undefined 
    typeOfOp Index xs = undefined 
    typeOfOp Mask xs = undefined 
    typeOfOp CopyNesting xs = undefined 
    typeOfOp Flatten xs = undefined 
    typeOfOp ConstVector xs = undefined 
    typeOfOp Sort [v,d] = Just v
    typeOfOp SortRank [v,d] = Just$ Tuple [v,Dense I ArBB.ArbbUsize] -- DVector Dim1 a -> (DVector Dim1 a, DVector Dim1 USize)
    typeOfOp Replace xs = undefined 
    typeOfOp SetRegularNesting xs = undefined 
    typeOfOp ReplaceRow xs = undefined 
    typeOfOp ReplaceCol xs = undefined 
    typeOfOp ReplacePage xs = undefined 
    typeOfOp GetNRows xs = undefined 
    typeOfOp GetNCols xs = undefined 
    typeOfOp GetNPages xs = undefined 
    typeOfOp ExtractRow xs = undefined 
    typeOfOp ExtractCol xs = undefined 
    typeOfOp ExtractPage xs = undefined 
    typeOfOp Section xs = undefined 
    typeOfOp Segment xs = undefined 
    typeOfOp ReplaceSegment xs = undefined 
    typeOfOp Alloc xs = undefined 
    typeOfOp ReplaceElem xs = undefined 
    typeOfOp GetEltCoord xs = undefined 
    typeOfOp BitwiseCast xs = undefined 
    typeOfOp GetNeighbor xs = undefined 
    typeOfOp ExpectSize xs = undefined 
    typeOfOp AddReduce [v,l] = decrRank v  
    typeOfOp MulReduce [v,l] = decrRank v 
    typeOfOp MaxReduce [v,l] = decrRank v 
    typeOfOp MaxReduceLoc xs = undefined 
    typeOfOp MinReduce [v,l] = decrRank v 
    typeOfOp MinReduceLoc xs = undefined 
    typeOfOp AndReduce [v,l] = decrRank v 
    typeOfOp IorReduce [v,l] = decrRank v 
    typeOfOp XorReduce [v,l] = decrRank v 
    typeOfOp AddScan [v,d,l] = Just v --Vec, USize, USise  
    typeOfOp MulScan [v,d,l] = Just v --Vec, USize, USise  
    typeOfOp MaxScan [v,d,l] = Just v --Vec, USize, USise  
    typeOfOp MinScan [v,d,l] = Just v --Vec, USize, USise  
    typeOfOp AndScan [v,d,l] = Just v --Vec, USize, USise  
    typeOfOp IorScan [v,d,l] = Just v --Vec, USize, USise  
    typeOfOp XorScan [v,d,l] = Just v --Vec, USize, USise  
    typeOfOp AddMerge xs = undefined 
    typeOfOp AddMergeScalar xs = undefined 
