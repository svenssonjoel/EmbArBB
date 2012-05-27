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

-- TODO: MUCH Cheating going on here. 
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
      
    typecheckLiteral (LitInt _)   = return$ Just$ Scalar ArBB.ArbbI64 -- FIX !   
    typecheckLiteral (LitInt8 _)   = return$ Just$ Scalar ArBB.ArbbI8
    typecheckLiteral (LitInt16 _)  = return$ Just$ Scalar ArBB.ArbbI16
    typecheckLiteral (LitInt32 _)  = return$ Just$ Scalar ArBB.ArbbI32
    typecheckLiteral (LitInt64 _)  = return$ Just$ Scalar ArBB.ArbbI64
    typecheckLiteral (LitWord _) = return$ Just$ Scalar ArBB.ArbbU64 -- FIX ! 
    typecheckLiteral (LitWord8 _) = return$ Just$ Scalar ArBB.ArbbU8
    typecheckLiteral (LitWord16 _) = return$ Just$ Scalar ArBB.ArbbU16
    typecheckLiteral (LitWord32 _) = return$ Just$ Scalar ArBB.ArbbU32
    typecheckLiteral (LitWord64 _) = return$ Just$ Scalar ArBB.ArbbU64
    typecheckLiteral (LitFloat _) = return$ Just$ Scalar ArBB.ArbbF32
    typecheckLiteral (LitDouble _) = return$ Just$ Scalar ArBB.ArbbF64
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
    typeOfOp Rcp [t] = Just t      
    typeOfOp Round [t] = Just t 
    typeOfOp Rsqrt [t] = Just t 
    typeOfOp Sin  [t] = Just t
    typeOfOp Sinh [t] = Just t 
    typeOfOp Sqrt [t] = Just t 
    typeOfOp Tan  [t] = Just t 
    typeOfOp Tanh [t] = Just t 
    typeOfOp Neg  [t] = Just t
    typeOfOp Bit_and [t1,t2] = same t1 t2  
    typeOfOp Bit_or  [t1,t2] = same t1 t2           
    typeOfOp Bit_xor [t1,t2] = same t1 t2
    typeOfOp Atan2 [t1,t2] = same t1 t2   
    typeOfOp Compare [t1,t2] = Just $ Scalar ArBB.ArbbIsize 
    typeOfOp Equal    _ = Just $ Scalar ArBB.ArbbBoolean
    typeOfOp Geq      _ = Just $ Scalar ArBB.ArbbBoolean
    typeOfOp Leq      _ = Just $ Scalar ArBB.ArbbBoolean
    typeOfOp Less [a,b] = Just $ Scalar ArBB.ArbbBoolean
    typeOfOp Log_and [t1,t2] = same t1 t2 -- Bool -> Bool -> Bool  
    typeOfOp Log_or  [t1,t2] = same t1 t2 -- Bool -> Bool -> Bool  
    typeOfOp Lsh [t1,t2] = Just t1  
    typeOfOp Mod [t1,t2] = Just t1 
    typeOfOp Neq [t1,t2] = Just $ Scalar ArBB.ArbbBoolean
    typeOfOp Pow [t1,t2] = same t1 t2 
    typeOfOp Rsh [t1,t2] = Just t1 
    typeOfOp Select [t1,t2,t3] = same t2 t3
    typeOfOp Gather (t:ts) = Just t    
    typeOfOp Scatter (t:ts) = Just t  
    typeOfOp Pack [t1,t2] = Just t1
    typeOfOp Unpack [t1,t2,t3] = Just t1
    typeOfOp Shuffle [t1,t2,t3,t4] = same t1 t2
    typeOfOp Unshuffle [t1,t2,t3] = Just t1 
    typeOfOp Repeat [t1,t2,t3] = Just t1
    typeOfOp Distribute (t:ts) = Just t
    typeOfOp RepeatRow [Dense _ t,t2] = Just $ Dense II t -- Just t1
    typeOfOp RepeatCol [t1,t2] = Just t1
    typeOfOp RepeatPage [t1,t2] = Just t1
    typeOfOp Transpose [t] = Just t
    typeOfOp SwapCol [t1,t2,t3] = Just t1
    typeOfOp SwapRow [t1,t2,t3] = Just t1
    typeOfOp SwapPage [t1,t2,t3] = Just t1
    typeOfOp ShiftConst (t:ts) = Just t
    typeOfOp ShiftClamp (t:ts) = Just t
    typeOfOp ShiftConstRev (t:ts) = Just t
    typeOfOp ShiftClampRev (t:ts) = Just t
    typeOfOp Rotate [v,d] = Just v 
    typeOfOp RotateRev [v,d] = Just v
    typeOfOp Reverse [v] = Just v 
    typeOfOp Length  [v] = Just $ Scalar ArBB.ArbbUsize
    typeOfOp ApplyNesting xs = undefined 
    typeOfOp GetNesting xs = undefined 
    typeOfOp Cat [t1,t2] = same t1 t2 

    -- TODO: Big Cheat ! 
    typeOfOp Cast xs = Just $ Dense I ArBB.ArbbUsize 
    -- TODO: Replace with  
    -- typeOfOf CastTo t xs = t 

    
    typeOfOp Extract [Dense I a,i] = Just $ Scalar a 
    typeOfOp Extract [Dense II a,i,j] = Just $ Scalar a 
    typeOfOp Extract [Dense III a,i,j,k] = Just $ Scalar a 
    
    typeOfOp Split xs = undefined    -- dense to nested 
    typeOfOp Unsplit xs = undefined  -- nested to dense
    typeOfOp Index xs = undefined    -- TODO: not sure what to do here!
    typeOfOp Mask xs = Just $ Dense I ArBB.ArbbBoolean
    typeOfOp CopyNesting xs = undefined -- nested
    typeOfOp Flatten xs = undefined     -- nested to dense
    typeOfOp ConstVector [Scalar t,_] = Just$ Dense I t  
    typeOfOp Sort [v,d] = Just v
    typeOfOp SortRank [v,d] = Just$ Tuple [v,Dense I ArBB.ArbbUsize] -- DVector Dim1 a -> (DVector Dim1 a, DVector Dim1 USize)
    typeOfOp Replace (t:ts) = Just t
    typeOfOp SetRegularNesting [Dense I t,t2,t3] = Just $ Dense II t
    typeOfOp SetRegularNesting [Dense II t,t2,t3,t4,t5] = Just $ Dense III t
    typeOfOp ReplaceRow [t1,t2,t3] = Just t1
    typeOfOp ReplaceCol [t1,t2,t3] = Just t1 
    typeOfOp ReplacePage [t1,t2,t3] = Just t1
    typeOfOp GetNRows xs = Just $ Scalar ArBB.ArbbUsize 
    typeOfOp GetNCols xs = Just $ Scalar ArBB.ArbbUsize 
    typeOfOp GetNPages xs = Just $ Scalar ArBB.ArbbUsize 
    typeOfOp ExtractRow [t1,t2] = decrRank t1
    typeOfOp ExtractCol  [t1,t2] = decrRank t1
    typeOfOp ExtractPage [t1,t2] = decrRank t1 
    typeOfOp Section (t:ts) = Just t
    typeOfOp Segment xs = undefined  -- Nested to dense
    typeOfOp ReplaceSegment xs = undefined  -- Nested to nested
    typeOfOp Alloc xs = undefined  -- TODO: not sure what to do here! 
    typeOfOp ReplaceElem (t:ts) = Just t
    typeOfOp GetEltCoord [] = Just $ Tuple [Scalar ArBB.ArbbUsize, 
                                            Scalar ArBB.ArbbUsize, 
                                            Scalar ArBB.ArbbUsize]
    typeOfOp BitwiseCast xs = undefined -- TODO: not sure what to do here! 
    typeOfOp GetNeighbor [t,t2,t3,t4] = Just $ t
    typeOfOp ExpectSize xs = undefined    -- used for optimization
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
    typeOfOp AddMerge [t1,t2,t3] = Just t1
    typeOfOp AddMergeScalar xs = undefined -- TODO: Don't know what this is
