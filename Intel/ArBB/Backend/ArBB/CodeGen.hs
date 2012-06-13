{- 2012 Joel Svensson -} 


module Intel.ArBB.Backend.ArBB.CodeGen (accmBody, 
                                        toArBBType,
                                        copyAll,
                                        typeToArBBGlobalVar) 
    where 

import Intel.ArBB.DAG
import Intel.ArBB.Syntax 
import Intel.ArBB.Literal
import Intel.ArBB.Variable
import Intel.ArBB.Op
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

type Gen a = TypeChecker (StateT (Map.Map NodeID [VM.Variable]) VM.EmitArbb) a 

liftVM = (lift.lift)


runGen :: Gen a -> VarType -> VM.EmitArbb a 
runGen g vt = fst `fmap` (runGen' g vt (Map.empty))
--  do 
--    let m = runTypeChecker g vt 
--    evalStateT m (Map.empty) 

type NodeIDVar = Map.Map NodeID [VM.Variable]

runGen' :: Gen a -> VarType -> NodeIDVar -> VM.EmitArbb (a,NodeIDVar) 
runGen' g vt nidv = 
  do 
    let m = runTypeChecker g vt 
    runStateT m nidv
    
type AllState = ((VarType,NodeIDType),NodeIDVar)

runGenAllState :: Gen a -> AllState -> VM.EmitArbb (a,AllState) 
runGenAllState g (vt,nv) = 
  do
    let m = runTypeChecker' g vt 
    ((a,vt'),nt') <- runStateT m nv 
    return (a, (vt',nt'))
    
    
    
---------------------------------------------------------------------------- 
-- attempt to define operations on new Gen a 

visited :: NodeID -> Gen (Maybe [VM.Variable]) 
visited nid = 
  do 
    m <- lift get 
    return$ Map.lookup nid m 

addNode :: NodeID -> [VM.Variable] -> Gen () 
addNode nid v = 
  do 
    m <- lift get 
    let m' = Map.insert nid v m 
    lift (put m')
    


getTypeOfNode :: NodeID {- -> NodeIDType -} -> Gen [VM.Type] 
getTypeOfNode n = 
  do 
    (_,nidt) <- get 
    let t = fromJust$ Map.lookup n nidt 
    liftVM $ toArBBType t -- huh ? 

getTypeOfNode' :: NodeID -> NodeIDType -> Gen Type 
getTypeOfNode' n m = return$ fromJust$ Map.lookup n m 

    
getAllState :: Gen AllState
getAllState = 
  do 
    a <- get 
    b <- lift get
    return (a,b)
    
----------------------------------------------------------------------------
-- Wrapper 
    
genBody :: DAG 
           -> NodeID -- -> NodeIDType 
           -> VarType 
           -> (Map.Map Integer (VM.ConvFunction,[Type],[Type]))
           -> (Map.Map Integer Integer) 
           -> [(Variable, VM.Variable)] 
           -> VM.EmitArbb [VM.Variable] 
-- genBody dag nid typem funm is = evalStateT (genBody' dag nid typem funm is) (Map.empty) 
genBody dag nid  vt funm depm is = runGen (genBody' dag nid funm depm is) vt 
 
accmBody :: DAG
            -> [NodeID] 
        --    -> NodeIDType 
            -> VarType
            -> (Map.Map Integer (VM.ConvFunction,[Type],[Type]))
            -> (Map.Map Integer Integer) -- dependency id to function id 
            -> [(Variable, VM.Variable)] 
            -> VM.EmitArbb [VM.Variable]
accmBody dag nids vt funm depm is = liftM fst $ doBody nids ((vt,Map.empty),Map.empty) 

  where 
    doBody ::[NodeID] -> AllState -> VM.EmitArbb ([VM.Variable],AllState)
    doBody [] m = return ([],m) 
    doBody (x:xs) m =
      do 
          -- liftIO$ putStrLn "doBody"
          (vs,s) <- runGenAllState (genBody' dag x  funm depm is) m
          (vss,s') <- doBody xs s
          return (vs++vss,s')
{-
    doBody ::[NodeID] -> (Map.Map NodeID [VM.Variable]) -> VM.EmitArbb ([VM.Variable],(Map.Map NodeID [VM.Variable]))
    doBody [] m = return ([],m) 
    doBody (x:xs) m =
      do 
          (vs,m') <- runGen' (genBody' dag x  funm is) vt m
          (vss,m'') <- doBody xs m'
          return (vs++vss,m'')
-}

accmBodyLocal :: DAG
            -> [NodeID] 
        --    -> NodeIDType 
            -> AllState
            -> (Map.Map Integer (VM.ConvFunction,[Type],[Type]))
            -> (Map.Map Integer Integer)
            -> [(Variable, VM.Variable)] 
            -> VM.EmitArbb [VM.Variable]
accmBodyLocal dag nids allstate funm depm is = liftM fst $ doBody nids allstate -- ((vt,Map.empty),Map.empty) 

  where 
    doBody ::[NodeID] -> AllState -> VM.EmitArbb ([VM.Variable],AllState)
    doBody [] m = return ([],m) 
    doBody (x:xs) m =
      do 
          (vs,s) <- runGenAllState (genBody' dag x funm depm is) m
          (vss,s') <- doBody xs s
          return (vs++vss,s')


----------------------------------------------------------------------------
-- The real worker



genBody' :: DAG 
           -> NodeID 
           -> (Map.Map Integer (VM.ConvFunction,[Type],[Type])) 
           -> (Map.Map Integer Integer) -- dependency id to function id 
           -> [(Variable,VM.Variable)] 
           -> Gen [VM.Variable] 
genBody' dag nid funm depm is = 
  do 
    m <- lift get 
    -- liftIO$ putStrLn $ "genBody' " ++ show nid
    -- liftIO$ putStrLn $ "genBody' " ++ show dag
    case Map.lookup nid m of 
      (Just v) -> 
        do 
          -- liftIO$ putStrLn$ "already generated : " ++ show nid 
            
          return v 
      Nothing   -> 
          case Map.lookup nid dag of 
            (Just node) -> 
              do 
                -- Update the "already generated" map
                v <- genNode nid node 
                (lift . put) (Map.insert nid v m)  
                return v
            Nothing -> error "genBody: DAG is broken" 
            
  where 
    genNode :: NodeID -> Node -> Gen [VM.Variable]
    genNode thisNid (NLit l) = 
      do 
        v <- genLiteral l 
        return [v] 
        
        -- Updated this is preparation for the For loop variables. 
    genNode thisNid (NVar v) =  --(Variable nom)) = 
      let v' = fromJust $ L.lookup v is 
      in return [v'] 
         -- [is !! (read (nom L.\\ "v") :: Int)]  -- inputs

    genNode thisNid (NResIndex n i) = 
      do 
        vs <- genBody' dag n  funm depm is 
        return [vs !! i]
    genNode thisNid (NIndex0 n) = 
      do 
        vs <- genBody' dag n  funm depm is 
        return vs
 
    genNode thisNid (NOp op ns) = 
      do 
        vs <- mapM (\n -> genBody' dag n funm depm is) ns 
       
        t <- typecheckNID dag thisNid
        imm <- liftVM$ typeToArBBLocalVar t 
    
        case isOpDynamic op of 
          True -> liftVM$ VM.opDynamic_ (opToArBB op) imm (concat vs)
          False -> liftVM$ VM.op_ (opToArBB op) imm (concat vs) 
        
        return imm
        
    ---------------------------------------------------    
    -- TODO: Is potentially duplicating work ?    
    --       See the sobel.hs example.
    genNode thisNid (NIf n1 n2 n3) =  
      do       
        -- conditional is a single var
        [v1] <- genBody' dag n1 funm depm is 
        -- TODO: condition variables need to be local. 
        bt <- liftVM$ VM.getScalarType_ VM.ArbbBoolean
        cond <- liftVM$ VM.createLocal_ bt "cond" 
        -- Ensure that cond is local 
        liftVM$ VM.copy_ cond v1     
        
        -- May be more than one result
        -- t <- getTypeOfNode' thisNid typem  
        t <- typecheckNID dag thisNid 

        imm <- liftVM$ typeToArBBLocalVar t 
      
        state  <- lift get -- use both states 
        (vt,_) <- get 
        liftVM$ VM.if_ cond 
          (do
              c1 <- fst `fmap` runGen' (genBody' dag n2 funm depm is) vt state
              copyAll imm c1              
          ) 
          (do
              c2 <- fst `fmap` runGen' (genBody' dag n3 funm depm is) vt state
              copyAll imm c2
          )
        addNode thisNid imm 
        return imm

    -- TODO: Extend typem with the types of the "lx" variables. 
    genNode thisNid a@(NWhile vars cond' body st) = 
      do 
     
        -- declare variables.  
        t   <- mapM (typecheckNID dag) st 
     
        -- liftIO$ putStrLn$ show t 
        state_vars <-  liftVM$ concat `fmap` ( mapM typeToArBBLocalVar t )
     
        -- extend the environment.. 
        let vis = is ++ (zip vars state_vars) 
      
        (vt,notused) <- get   
        let newVT = Map.union vt (Map.fromList (zip vars t))
        -- liftIO$ putStrLn $ show newVT
        put (newVT,notused) -- huh ?
        -- t'  <- mapM (typecheckNID dag) body -- ensure locals exist in typemap
        -- t'' <- typecheckNID dag cond'
      
        
        -- process initial state
        vs <- liftVM$ accmBody dag st vt funm depm vis 
      
        liftVM$ copyAll state_vars vs
        
        --TODO: Figure out what is so special about the loop variable 
        --      If anything.. (things break down here)
        
        alls <- getAllState 
        -- the actual loop 
        liftVM$ VM.while_ (do [c] <- accmBody dag [cond'] newVT funm depm vis; return c ) 
           ( do 
                c1 <- accmBodyLocal dag body alls funm depm vis
                copyAll state_vars c1 -- update state
           )
        
        addNode thisNid state_vars -- state_vars
        return state_vars
    ------------------------------------------------------------------- 
    -- TODO: This one should perform many checks.. 
    --       There are quite a number of things that have to match up 
    --       in an ArBB map call. 
    -- TODO: Can map be used with nested vectors? 
    genNode thisNid (NMap f ns) = 
      do 
        vs <- liftM concat $ mapM (\n -> genBody' dag n funm depm is) ns 
       
        -- In order to obtain dimensionality of result. 
        ts <- mapM (typecheckNID dag) ns
        let    dim = (\(Dense d _) -> d) $ head ts 
        
        let (Just funId) = Map.lookup f depm
        let (Just (fun,ti,to)) = Map.lookup funId funm
      
        -- TODO: All this starts feeling a bit "hacky".
        --       Put adding structure to the TODO list 
 
        -- TODO: Look this code over again.
        imm <- liftVM$ 
               liftM concat $ 
               mapM typeToArBBLocalVar $ 
               map (makeDenseType (\s -> Dense dim s)) to
        
    
        liftVM $ VM.map_ fun imm vs 
        
        return imm
        
makeDenseType :: (VM.ScalarType -> Type) -> Type -> Type 
makeDenseType f (Scalar t) = f t 
makeDenseType f _ = error "denseType applied to nonscalar"        

genLiteral :: Literal -> Gen VM.Variable
genLiteral (LitInt8 i)  = liftVM$ VM.int8_ i 
genLiteral (LitInt16 i) = liftVM$ VM.int16_ i 
genLiteral (LitInt32 i) = liftVM$ VM.int32_ i 
genLiteral (LitInt64 i) = liftVM$ VM.int64_ i
genLiteral (LitWord8 i) = liftVM$ VM.uint8_ i 
genLiteral (LitWord16 i) = liftVM$ VM.uint16_ i 
genLiteral (LitWord32 i) = liftVM$ VM.uint32_ i 
genLiteral (LitWord64 i) = liftVM$ VM.uint64_ i 
genLiteral (LitFloat i) = liftVM$ VM.float32_ i 
genLiteral (LitDouble i) = liftVM$ VM.float64_ i 
genLiteral (LitISize (ISize i)) = liftVM$ VM.isize_ i
genLiteral (LitUSize (USize i)) = liftVM$ VM.usize_ i
genLiteral (LitBool  b) = liftVM$ VM.bool_ b 

opToArBB :: Op -> VM.Opcode
opToArBB Add = VM.ArbbOpAdd
opToArBB Mul = VM.ArbbOpMul 
opToArBB Sub = VM.ArbbOpSub
opToArBB Div = VM.ArbbOpDiv
opToArBB Min = VM.ArbbOpMin
opToArBB Max = VM.ArbbOpMax
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
opToArBB Gather = VM.ArbbOpGather 
opToArBB Scatter = VM.ArbbOpScatter
opToArBB Pack = VM.ArbbOpPack
opToArBB Unpack = VM.ArbbOpUnpack 
opToArBB Shuffle = VM.ArbbOpShuffle
opToArBB Unshuffle = VM.ArbbOpUnshuffle
opToArBB Repeat = VM.ArbbOpRepeat  
opToArBB Distribute = VM.ArbbOpDistribute 
opToArBB RepeatRow = VM.ArbbOpRepeatRow  
opToArBB RepeatCol = VM.ArbbOpRepeatCol 
opToArBB RepeatPage = VM.ArbbOpRepeatPage
opToArBB Transpose = VM.ArbbOpTranspose 
opToArBB SwapCol = VM.ArbbOpSwapCol 
opToArBB SwapRow = VM.ArbbOpSwapRow
opToArBB SwapPage= VM.ArbbOpSwapPage 
opToArBB ShiftConst = VM.ArbbOpShiftConstant 
opToArBB ShiftClamp = VM.ArbbOpShiftClamp
opToArBB ShiftConstRev = VM.ArbbOpShiftConstantReverse 
opToArBB ShiftClampRev = VM.ArbbOpShiftClampReverse
opToArBB Rotate = VM.ArbbOpRotate
opToArBB RotateRev = VM.ArbbOpRotateReverse 
opToArBB Reverse = VM.ArbbOpReverse
opToArBB Length = VM.ArbbOpLength 
opToArBB ApplyNesting = VM.ArbbOpApplyNesting 
opToArBB GetNesting = VM.ArbbOpGetNesting 
opToArBB Cat = VM.ArbbOpCat 
-- Just throw away the Cast's type info
opToArBB (Cast _) = VM.ArbbOpCast 
opToArBB Extract = VM.ArbbOpExtract 
opToArBB Split = VM.ArbbOpSplit 
opToArBB Unsplit = VM.ArbbOpUnsplit 
opToArBB Index = VM.ArbbOpIndex 
opToArBB Mask = VM.ArbbOpMask 
opToArBB CopyNesting = VM.ArbbOpCopyNesting 
opToArBB Flatten = VM.ArbbOpFlatten 
opToArBB ConstVector = VM.ArbbOpConstVector 
opToArBB Sort = VM.ArbbOpSort 
opToArBB SortRank = VM.ArbbOpSortRank 
opToArBB Replace = VM.ArbbOpReplace 
opToArBB SetRegularNesting = VM.ArbbOpSetRegularNesting 
opToArBB ReplaceRow = VM.ArbbOpReplaceRow 
opToArBB ReplaceCol = VM.ArbbOpReplaceCol 
opToArBB ReplacePage = VM.ArbbOpReplacePage 
opToArBB GetNRows = VM.ArbbOpGetNrows 
opToArBB GetNCols = VM.ArbbOpGetNcols 
opToArBB GetNPages = VM.ArbbOpGetNpages 
opToArBB ExtractRow = VM.ArbbOpExtractRow 
opToArBB ExtractCol = VM.ArbbOpExtractCol 
opToArBB ExtractPage = VM.ArbbOpExtractPage
opToArBB Section = VM.ArbbOpSection 
opToArBB Segment = VM.ArbbOpSegment 
opToArBB ReplaceSegment = VM.ArbbOpReplaceSegment 
opToArBB Alloc = VM.ArbbOpAlloc 
opToArBB ReplaceElem = VM.ArbbOpReplaceElement 
opToArBB GetEltCoord = VM.ArbbOpGetEltCoord
opToArBB BitwiseCast = VM.ArbbOpBitwiseCast 
opToArBB GetNeighbor = VM.ArbbOpGetNeighbor 
opToArBB ExpectSize = VM.ArbbOpExpectSize 
opToArBB AddReduce = VM.ArbbOpAddReduce 
opToArBB MulReduce = VM.ArbbOpMulReduce  
opToArBB MaxReduce = VM.ArbbOpMaxReduce 
opToArBB MaxReduceLoc = VM.ArbbOpMaxReduceLoc 
opToArBB MinReduce = VM.ArbbOpMinReduce 
opToArBB MinReduceLoc = VM.ArbbOpMinReduceLoc 
opToArBB AndReduce = VM.ArbbOpAndReduce 
opToArBB IorReduce = VM.ArbbOpIorReduce 
opToArBB XorReduce = VM.ArbbOpXorReduce 
opToArBB AddScan = VM.ArbbOpAddScan 
opToArBB MulScan = VM.ArbbOpMulScan 
opToArBB MaxScan = VM.ArbbOpMaxScan 
opToArBB MinScan = VM.ArbbOpMinScan 
opToArBB AndScan = VM.ArbbOpAndScan 
opToArBB IorScan = VM.ArbbOpIorScan 
opToArBB XorScan = VM.ArbbOpXorScan 
opToArBB AddMerge = VM.ArbbOpAddMerge 
opToArBB AddMergeScalar = VM.ArbbOpAddMergeScalar 
      
-- TODO: there are many Dynamnic ops here that are incorrectly "False"                     
isOpDynamic :: Op -> Bool
isOpDynamic Add = False 
isOpDynamic Mul = False 
isOpDynamic Sub = False 
isOpDynamic Div = False
isOpDynamic Min = False 
isOpDynamic Max = False 
isOpDynamic Acos = False
isOpDynamic Asin = False
isOpDynamic Atan = False
isOpDynamic Ceil = False
isOpDynamic Cos = False 
isOpDynamic Cosh = False
isOpDynamic Exp = False 
isOpDynamic Exp10 = False 
isOpDynamic Floor = False 
isOpDynamic Ln  = False 
isOpDynamic Log10 = False 
isOpDynamic Log_not = False 
isOpDynamic Bit_not = False 
isOpDynamic Rcp = False 
isOpDynamic Round = False 
isOpDynamic Rsqrt = False 
isOpDynamic Sin  = False 
isOpDynamic Sinh = False 
isOpDynamic Sqrt = False 
isOpDynamic Tan = False 
isOpDynamic Tanh = False
isOpDynamic Neg = False 
isOpDynamic Bit_and  = False 
isOpDynamic Atan2 = False 
isOpDynamic Compare = False 
isOpDynamic Equal = False 
isOpDynamic Geq = False 
isOpDynamic Bit_or = False 
isOpDynamic Leq = False 
isOpDynamic Less = False 
isOpDynamic Log_and = False 
isOpDynamic Log_or = False 
isOpDynamic Lsh = False 
isOpDynamic Mod = False 
isOpDynamic Neq = False 
isOpDynamic Pow = False 
isOpDynamic Rsh = False 
isOpDynamic Bit_xor = False 
isOpDynamic Select = False 
isOpDynamic Gather = True  
isOpDynamic Scatter = True 
isOpDynamic Pack = False 
isOpDynamic Unpack = False  
isOpDynamic Shuffle = False 
isOpDynamic Unshuffle = False 
isOpDynamic Repeat = False   
isOpDynamic Distribute = True
isOpDynamic RepeatRow = False   
isOpDynamic RepeatCol = False  
isOpDynamic RepeatPage = False
isOpDynamic Transpose = False  
isOpDynamic SwapCol = False  
isOpDynamic SwapRow = False 
isOpDynamic SwapPage = False  
isOpDynamic ShiftConst = True
isOpDynamic ShiftClamp = True
isOpDynamic ShiftConstRev = True 
isOpDynamic ShiftClampRev = True
isOpDynamic Rotate = True
isOpDynamic RotateRev = True  
isOpDynamic Reverse = False 
isOpDynamic Length = False  
isOpDynamic ApplyNesting = False  
isOpDynamic GetNesting = False  
isOpDynamic Cat = False  
isOpDynamic (Cast t) = False  
isOpDynamic Extract = True 
isOpDynamic Split = False  
isOpDynamic Unsplit = False  
isOpDynamic Index = False  
isOpDynamic Mask = False  
isOpDynamic CopyNesting = False  
isOpDynamic Flatten = False  
isOpDynamic ConstVector = False  
isOpDynamic Sort = False  
isOpDynamic SortRank = False  
isOpDynamic Replace = True  
isOpDynamic SetRegularNesting = True
isOpDynamic ReplaceRow = False  
isOpDynamic ReplaceCol = False  
isOpDynamic ReplacePage = False  
isOpDynamic GetNRows = False  
isOpDynamic GetNCols = False  
isOpDynamic GetNPages = False  
isOpDynamic ExtractRow = False  
isOpDynamic ExtractCol = False  
isOpDynamic ExtractPage = False 
isOpDynamic Section = True  
isOpDynamic Segment = False  
isOpDynamic ReplaceSegment = True 
isOpDynamic Alloc = False  
isOpDynamic ReplaceElem = False  
isOpDynamic GetEltCoord = False 
isOpDynamic BitwiseCast = False  
isOpDynamic GetNeighbor = False  
isOpDynamic ExpectSize = False  
isOpDynamic AddReduce = True  
isOpDynamic MulReduce = True   
isOpDynamic MaxReduce = True 
isOpDynamic MaxReduceLoc = True  
isOpDynamic MinReduce = True  
isOpDynamic MinReduceLoc = True  
isOpDynamic AndReduce = True
isOpDynamic IorReduce = True  
isOpDynamic XorReduce = True  
isOpDynamic AddScan = False  
isOpDynamic MulScan = False  
isOpDynamic MaxScan = False  
isOpDynamic MinScan = False  
isOpDynamic AndScan = False  
isOpDynamic IorScan = False  
isOpDynamic XorScan = False  
isOpDynamic AddMerge = False  
isOpDynamic AddMergeScalar = False  



-- valid reduction op ? 
opToArBBReduceOp Add = VM.ArbbOpAddReduce
opToArBBReduceOp Mul = VM.ArbbOpMulReduce
opToArBBReduceOp Max = VM.ArbbOpMaxReduce
opToArBBReduceOp Min = VM.ArbbOpMinReduce
opToArBBReduceOp And = VM.ArbbOpMinReduce
opToArBBReduceOp Ior = VM.ArbbOpIorReduce
opToArBBReduceOp Xor = VM.ArbbOpXorReduce

-- valid scan op ? 
opToArBBScanOp Add = VM.ArbbOpAddScan
opToArBBScanOp Mul = VM.ArbbOpMulScan
opToArBBScanOp Max = VM.ArbbOpMaxScan
opToArBBScanOp Min = VM.ArbbOpMinScan
opToArBBScanOp And = VM.ArbbOpMinScan
opToArBBScanOp Ior = VM.ArbbOpIorScan
opToArBBScanOp Xor = VM.ArbbOpXorScan



----------------------------------------------------------------------------   
-- 

-- | Turn a Type into a opaque ArBB-VM type object
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


-- | declare variables to hold data of a given Type. 
typeToArBBGlobalVar :: Type -> VM.EmitArbb [VM.Variable]
typeToArBBGlobalVar t = 
  do 
    ts <- toArBBType t 
    gs <- mapM (\t -> VM.createGlobal_nobind_ t "noname") ts 
    ys <- mapM VM.variableFromGlobal_ gs 
    return ys 

    
-- | declare local
typeToArBBLocalVar :: Type -> VM.EmitArbb [VM.Variable]
typeToArBBLocalVar t = 
  do 
    ts <- toArBBType t 
    ys <- mapM (\t -> VM.createLocal_ t "noname") ts 
    return ys 
    

copyAll [] [] = return ()    
copyAll (x:xs) (y:ys) = 
  do 
    VM.copy_ x y  
    copyAll xs ys 
--copyAll xs ys = error$ show xs ++ " " ++ show ys 




