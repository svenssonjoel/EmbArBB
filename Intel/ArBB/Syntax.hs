{-# LANGUAGE TypeOperators, 
             FlexibleInstances,
           --  ExistentialQuantification, 
             DeriveDataTypeable #-}

{- 2012 Joel Svensson -} 

module Intel.ArBB.Syntax where 

import Intel.ArBB.Types
import Intel.ArBB.Literal 
import Intel.ArBB.Variable
import Intel.ArBB.Op

import Intel.ArBB.MonadCapture
import Intel.ArBB.GenRecord

import Data.Typeable

----------------------------------------------------------------------------
--  Expression type. 
--   Will try to discover the sharing using the 
--   StableName method. (System.Mem.StableName)
data Expr = Lit Literal
          | Var Variable 
            
          | Index0 Expr 
            -- ArBB Functions may compute several results 
          | ResIndex Expr Int 
            
            -- Function with correct name and type must exist in some kind of environment
          | Call (Capture GenRecord) [Expr]  
          | Map  (Capture GenRecord) [Expr]   
          
          -- Hoas for the while loop.. 
          | While ([Expr] -> Expr)  ([Expr] -> [Expr])  [Expr] 
                             
          | If Expr Expr Expr -- could have been an op 

          | Op Op [Expr]   


            deriving (Typeable)
--           deriving (show)
  

---------------------------------------------------------------------------- 
-- add a layer of types 

data Exp a = E {unE :: Expr}
