

module Intel.ArBB.TypeCheck where 


{- Typecheck a DAG. and create a nodeID_to_type map -} 


import Intel.ArBB.DAG
import Intel.ArBB.Syntax

import qualified Intel.ArbbVM as ArBB

import Control.Monad.State 
import qualified Data.Map as Map 


---------------------------------------------------------------------------- 
-- What types can we have ? 
data Type = Scalar ArBB.ScalarType 
          | Dense Dimensionality ArBB.ScalarType 
          deriving (Eq,Show)                   

data Dimensionality = I | II | III 
                    deriving (Eq, Show)


isScalar :: Type -> Bool 
isScalar (Scalar _) = True
isScalar _ = False 

is1D :: Type -> Bool 
is1D (Dense I _) = True 
is1D _ = False 

is2D (Dense II _) = True 
is2D _ = False 

is3D (Dense III _) = True
is3D _ = False

decrRank (Dense III a) = Just$ Dense II a 
decrRank (Dense II a) = Just$ Dense I a 
decrRank (Dense I a) = Just$ Scalar a 
decrRank a = Nothing 

---------------------------------------------------------------------------- 
-- Typechecking state
type VarType      = Map.Map Variable Type 

type NodeIDType   = Map.Map NodeID Type 

type CheckState a = State (VarType,NodeIDType) a 



varType :: Variable -> CheckState (Maybe Type) 
varType v = 
  do 
    varsTypes <- liftM fst $ get 
    return $ Map.lookup v varsTypes 
  
addNodeIDType :: NodeID -> Type -> CheckState () 
addNodeIDType n t = 
  do 
    (v,nodts) <- get
    let nodts' = Map.insert n t nodts 
    put (v,nodts')

----------------------------------------------------------------------------
-- The type checker
-- Knowing the types at every node is required to generate the ArBB code. 
typecheckDAG :: DAG -> VarType -> NodeIDType
typecheckDAG dag vt = 
  snd $ snd $ runState (mapM_ (typecheckNID dag) (Map.keys dag)) (vt,Map.empty) 

-- helper               start
typecheckNID :: DAG -> NodeID -> CheckState Type
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
    typecheckNode dag (NBinOp op n1 n2) = 
      do 
        nt1 <- typecheckNID dag n1 
        nt2 <- typecheckNID dag n2
        -- Be more serious here later. 
        if nt1 == nt2 
          then return$ Just nt1 
          else return Nothing 
    typecheckNode dag (NUnOp op n) =                 
      do 
        -- Be more serious later.
        nt <- typecheckNID dag n 
        return$ Just$ nt 
    typecheckNode dag (NIndex0 n) = 
      do 
        nt <- typecheckNID dag n 
        if isScalar nt 
          then return$ Just$ nt 
          else return Nothing 
    typecheckNode dag (NIndex1 n1 n2) = 
      do 
        nt1 <- typecheckNID dag n1 
        nt2 <- typecheckNID dag n2 -- ignore now 
                                   -- earlier layer will ensure it is of right type
        if is1D nt1 
          then 
            let (Dense I scal) = nt1 
            in return$ Just $ Scalar scal 
          else return Nothing 
    typecheckNode dag (NIndex2 n1 n2 n3) = 
      do 
        nt1 <- typecheckNID dag n1 
        nt2 <- typecheckNID dag n2 -- ignore now 
                                   -- earlier layer will ensure it is of right type
        nt3 <- typecheckNID dag n3
        if is2D nt1 
          then 
            let (Dense II scal) = nt1 
            in return$ Just $ Scalar scal 
          else return Nothing            
    typecheckNode dag (NIndex3 n1 n2 n3 n4) = 
      do 
        nt1 <- typecheckNID dag n1 
        nt2 <- typecheckNID dag n2 -- ignore now 
                                   -- earlier layer will ensure it is of right type
        nt3 <- typecheckNID dag n3
        nt4 <- typecheckNID dag n4 
        if is3D nt1 
          then 
            let (Dense III scal) = nt1 
            in return$ Just $ Scalar scal 
          else return Nothing                       
    typecheckNode dag (NReduce op n) = 
      do 
        nt <- typecheckNID dag n 
        
        -- Be more serious later. 
        return $ decrRank nt 
        
    typecheckNode dag (NRotate n d) = 
      do 
        nt <- typecheckNID dag n     -- fix later
        dt <- typecheckNID dag d 
        return$ Just nt 
    typecheckNode dag (NRotateRev n d) = 
      do 
        nt <- typecheckNID dag n  -- fix later 
        dt <- typecheckNID dag d 
        return$ Just nt
      
    typecheckLiteral (LitInt8 _)   = return$ Just$ Scalar ArBB.ArbbI8
    typecheckLiteral (LitInt16 _)  = return$ Just$ Scalar ArBB.ArbbI16
    typecheckLiteral (LitInt32 _)  = return$ Just$ Scalar ArBB.ArbbI32
    typecheckLiteral (LitWord32 _) = return$ Just$ Scalar ArBB.ArbbU32
    
