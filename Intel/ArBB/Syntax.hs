{-# LANGUAGE TypeOperators, 
             FlexibleInstances,
             DeriveDataTypeable, 
             TypeSynonymInstances #-}

{- 2012 Joel Svensson -} 

module Intel.ArBB.Syntax where 


import Intel.ArBB.Literal 
import Intel.ArBB.Variable
import Intel.ArBB.Op

import Intel.ArBB.MonadReify
import Intel.ArBB.GenRecord

import Data.Typeable


----------------------------------------------------------------------------
-- Structure 

data Structure = StructSingle Expr 
               | StructTuple [Structure] 
                 deriving Show 

class Struct a where 
    struct   :: a -> Structure 
    destruct :: Structure -> a 

instance Struct Expr where 
    struct a = StructSingle a 
    destruct (StructSingle a) = a 

instance Struct (Exp a) where 
    struct a = StructSingle (unE a) 
    destruct (StructSingle a) = E a  

instance (Struct a , Struct b) => Struct (a,b) where 
    struct (a,b) = StructTuple [struct a, struct b] 
    destruct (StructTuple [a,b]) = (destruct a, destruct b) 


----------------------------------------------------------------------------
--  Expression type. 
--   Will try to discover the sharing using the 
--   StableName method. (System.Mem.StableName)
data Expr = Lit Literal
          | Var Variable 
            
          | Index0 Expr 
            -- ArBB Functions may compute several results 
          | ResIndex Expr Int 
            
          -- Function Call and Map.
          | Call (R GenRecord) [Expr]  
          | Map  (R GenRecord) [Expr]   
          
          -- Hoas for the while loop.. 
          -- Todo: Structure instead of [Expr] (Allow tuples etc) 
          | While ([Expr] -> Expr)  ([Expr] -> [Expr])  [Expr] 

          -- Sketch. TODO: Implement in the backend. 
          --   Come up with a traversal order for structures. Want to give 
          --   flat lists of things to ArBB.
          | While2 (Structure -> Expr)  
                   (Structure -> Structure)  
                   Structure 
          
          -- TODO: would anything be gained from adding the two other 
          --       kinds of ArBB loops. (For and Do loop). 

            
          | If Expr Expr Expr -- could have been an op 

          | Op Op [Expr]   


            deriving (Show,Typeable)

instance Show (R a) where 
    show a = "REIFY" 
instance Show (a -> b) where 
    show a = "FUNC" 

  

---------------------------------------------------------------------------- 
-- add a layer of types 

data Exp a = E {unE :: Expr}
