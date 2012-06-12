
module Intel.ArBB.MonadCapture where 

import Intel.ArBB.DAG
import Intel.ArBB.Variable
import Intel.ArBB.Types

import Control.Monad.State.Strict
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.Dynamic as Dynamic 



data CaptState = CaptState { sharing :: IntMap.IntMap [(Dynamic,Integer)]
                           , types   :: Map.Map Variable Type -- VarType
                           , unique  :: Integer 
                           , dag     :: DAG }  
               deriving Show

type Capture backend a = StateT CaptState backend a 