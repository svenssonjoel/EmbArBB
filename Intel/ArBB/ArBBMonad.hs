module Intel.ArBB.ArBBMonad where 

import Control.Monad.State.Strict hiding (liftIO) 
import qualified Data.Map as Map

import qualified Intel.ArbbVM as VM
import qualified Intel.ArbbVM.Convenience as VM

import Intel.ArBB.Types

type ArBB a = StateT ArBBState VM.EmitArbb a  

type ArBBState = ( Map.Map String (VM.ConvFunction, [Type], [Type])
                 , Map.Map Integer VM.Variable -- dVectorID to ArBB vector map
                 , Integer)