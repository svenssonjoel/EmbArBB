{-# LANGUAGE FlexibleInstances #-}

{- 2012 Joel Svensson -} 

module Intel.ArBB.DAG where 

import Intel.ArBB.Literal
import Intel.ArBB.Variable
import Intel.ArBB.Op

import Data.Word
import Data.Int
import qualified Data.Map as Map

import Data.Hashable 
import Data.Text

import Prelude as P 

---------------------------------------------------------------------------- 
-- DAG
type NodeID = Integer -- Word32

data Node = NLit Literal
          | NVar Variable 
          
          | NIndex0 NodeID
          
          | NResIndex NodeID Int 
          | NCall Integer [NodeID] 
          | NMap  Integer [NodeID]
            
          | NIf NodeID NodeID NodeID
          
          | NOp Op [NodeID] 
            
          | NWhile [Variable] NodeID [NodeID] [NodeID]
            
          deriving (Eq,Show)

type DAG = Map.Map NodeID Node

----------------------------------------------------------------------------
-- Hash DAGs

-- Probably horrible!
instance Hashable Node where 
    hash n = hash $ pack $ show n
    hashWithSalt i n = hashWithSalt i (pack (show n)) 


instance Hashable (Map.Map NodeID Node)  where 
    hash dag = P.foldl hashWithSalt 0 (Map.toList dag)