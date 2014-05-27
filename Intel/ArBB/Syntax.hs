{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE DeriveDataTypeable #-} 
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- 2012 Joel Svensson -} 

{- Notes:

   Changes 2014 
-} 

module Intel.ArBB.Syntax where 


import Intel.ArBB.Literal 
import Intel.ArBB.Variable
import Intel.ArBB.Op

-- import Intel.ArBB.MonadReify
-- import Intel.ArBB.GenRecord

import Data.Typeable
import Data.Supply 
import qualified Data.Set as S

import Control.Monad.State
import Control.Monad.Writer
----------------------------------------------------------------------------
-- Structure 

data Structure = StructSingle Expr 
               | StructTuple [Structure] 
                 deriving (Show) 

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


data Fun = Lam Variable Fun
         | Body Expr
         deriving (Show) 

class RFun f where
  toFun :: Supply Int -> f -> Fun

instance RFun Expr where
  toFun s e = Body e

instance RFun (Expr -> Expr) where 
  toFun s f = let i = supplyValue s
                  v = Variable$ "fun_local_v"++show v
                  body = f (Var v) 
              in Lam v (Body body)
                 
instance RFun b => RFun (Expr -> b) where
  toFun s f = let (s1,s2) = split2 s
                  i = supplyValue s1
                  v = Variable$ "fun_local_v"++show v
                  rest = toFun s2 $ f (Var v) 
              in Lam v rest
----------------------------------------------------------------------------
-- Free Variables

freeVarsFun :: Fun -> S.Set Variable
freeVarsFun (Body e) = freeVarsExp e
freeVarsFun (Lam v f) =  v `S.delete` freeVarsFun f

freeVarsExp (Var v) = S.singleton v
freeVarsExp (Index0 e) = freeVarsExp e
freeVarsExp (ResIndex e i) = freeVarsExp e
freeVarsExp (Map f es) = S.unions (map freeVarsExp es) `S.union` freeVarsFun f
freeVarsExp (Call f es) = S.unions (map freeVarsExp es) `S.union` freeVarsFun f
freeVarsExp (While c b s) = S.empty -- Fix this, surely not empty.
freeVarsExp (While2 c b s) = S.empty
freeVarsExp (If e1 e2 e3) = freeVarsExp e1 `S.union`
                            freeVarsExp e2 `S.union`
                            freeVarsExp e3
freeVarsExp (Op _ es) = S.unions (map freeVarsExp es) 


  
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
          -- Should also work with a ([Expr] -> Expr) function
          -- (test in a branch. Talk to Josef if trouble)
          -- Maybe needs to be generlised some ?   
          | Map Fun [Expr] 
          | Call Fun [Expr]

          -- | Call (R GenRecord) [Expr]  
          -- | Map  (R GenRecord) [Expr]   
          
          -- Hoas for the while loop.. 
          -- Todo: Structure instead of [Expr] (Allow tuples etc)
          -- Could use Fun representations as well... 
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

instance Show (a -> b) where 
    show a = "FUNC" 

  

---------------------------------------------------------------------------- 
-- add a layer of types 

data Exp a = E {unE :: Expr}
