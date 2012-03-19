{- 2012 Joel Svensson -} 

module Intel.ArBB.WithArBB where 

import Control.Monad.State.Strict 
import qualified Data.Map as Map

import qualified Intel.ArbbVM as VM
import qualified Intel.ArbbVM.Convenience as VM




import Intel.ArBB.Syntax (FunctionName)


----------------------------------------------------------------------------
--

data ArBBState = ArBBState (Map.Map FunctionName VM.ConvFunction) 
                           Integer 
                            

type ArBB a = StateT ArBBState VM.EmitArbb a  

liftIO :: IO a -> ArBB a 
liftIO = lift . VM.liftIO

withArBB :: ArBB a -> IO a 
withArBB arbb = 
  do 
    VM.arbbSession$  evalStateT arbb (ArBBState Map.empty 0)