{-# LANGUAGE MultiParamTypeClasses #-} 

module Intel.ArBB.Backend where 


type FuncID = Int

type VarID  = Int


data BackendState funType varType 
    = BackendState {beFunMap :: Map.Map FuncID funType, 
                    beVarMap :: Map.Map VarID  varType
                    beUnique :: Int}


class Monad m => BackendMonad m funType varType where 
    addFunction :: funType -> m FuncID
    lookFunction :: FuncID -> m funType 

    addVariable :: varType -> m VarID
    lookVariable :: VarID -> m varType 


