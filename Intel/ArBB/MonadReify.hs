{-# Language ScopedTypeVariables, 
             GeneralizedNewtypeDeriving #-} 
module Intel.ArBB.MonadReify where 

import Intel.ArBB.DAG
import Intel.ArBB.Variable
import Intel.ArBB.Types

import Intel.ArBB.GenRecord

import Control.Monad.State.Strict
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.Dynamic as Dynamic 

data RState = RState { sharing   :: IntMap.IntMap [(Dynamic,Integer)]
                     , unique    :: Integer 
                     , genRecord :: GenRecord } 
              deriving Show

type R a = StateT RState IO a
 

