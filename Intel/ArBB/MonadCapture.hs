{-# Language ScopedTypeVariables, 
             GeneralizedNewtypeDeriving #-} 
module Intel.ArBB.MonadCapture where 

import Intel.ArBB.DAG
import Intel.ArBB.Variable
import Intel.ArBB.Types
-- import Intel.ArBB.Backend.ArBB

import Control.Monad.State.Strict
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.Dynamic as Dynamic 

import qualified Intel.ArbbVM as VM
import qualified Intel.ArbbVM.Convenience as VM hiding (liftIO) 

data CaptState = CaptState { sharing :: IntMap.IntMap [(Dynamic,Integer)]
                           , types   :: Map.Map Variable Type -- VarType
                           , unique  :: Integer 
                           , dag     :: DAG }  
               deriving Show

type Capture a = StateT CaptState ArBBBackend a 


newtype ArBBBackend a = ArBBBackend {unArBBBackend :: (StateT ArBBState VM.EmitArbb a)}
    deriving (Monad, MonadState ArBBState, MonadIO, Functor) 

-- String FunctionName or FunctionID
type ArBBState = ( Map.Map String (VM.ConvFunction, [Type], [Type])
                 , Map.Map Integer VM.Variable -- dVectorID to ArBB vector map
                 , Integer)


--type FuncID = Integer 
--type VarID  = Integer 

