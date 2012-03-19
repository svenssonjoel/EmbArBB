{- 2012 Joel Svensson -} 

module Intel.ArBB.WithArBB where 

import Control.Monad.State.Strict 
import qualified Data.Map as Map

import qualified Intel.ArbbVM as VM
import qualified Intel.ArbbVM.Convenience as VM




import Intel.ArBB.Syntax (FunctionName)


----------------------------------------------------------------------------
--

type ArBBState = (Map.Map FunctionName VM.ConvFunction, Integer)
                            
addFunction :: FunctionName -> VM.ConvFunction -> ArBB () 
addFunction nom cf = do 
  (m,i) <- get 
  let m' = Map.insert nom cf m 
  put (m',i) 

getFunName :: ArBB FunctionName
getFunName = do 
  (m,i) <- get 
  put (m,i+1) 
  return $ "f" ++ show i 

type ArBB a = StateT ArBBState VM.EmitArbb a  

liftIO :: IO a -> ArBB a 
liftIO = lift . VM.liftIO

liftVM :: VM.EmitArbb a -> ArBB a 
liftVM = lift 

withArBB :: ArBB a -> IO a 
withArBB arbb = 
  do 
    VM.arbbSession$  evalStateT arbb (Map.empty,0)