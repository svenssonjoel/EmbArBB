
module Intel.ArBB.GenRecord where 

import Intel.ArBB.DAG
import Intel.ArBB.Types 
import Intel.ArBB.Variable

import qualified Data.Map as Map


-- a GenRecord should contain everything that the backend needs 
-- to generate a function and cache a function. 
-- If the function contains "maps" of other functions 
--  those other functions can be generated from the genRecordDepends Map.
data GenRecord = GenRecord { genRecordDag     :: DAG
                           , genRecordFunType :: Type 
                           , genRecordNids    :: [NodeID]
                           , genRecordVarType :: Map.Map Variable Type 
                           , genRecordDepends :: Map.Map Integer GenRecord}
                 deriving Show

emptyGenRecord = GenRecord Map.empty
                           undefined --- dont know yet
                           [] 
                           Map.empty
                           Map.empty

{- About the GenRecord. 
   
   the genRecordDepends map contains integer to genrecords mappings 
   for all functions called by the current function. 
   - A Map or Call node in the DAG (NMap Integer [Expr]),
     here the Integer points out one element in the genRecordDepends map. 
 
-}
    
      
