{-# LANGUAGE TypeOperators, 
             FlexibleInstances,
             TypeFamilies #-}

module Intel.ArBB.Function where 

import Intel.ArBB.Syntax

type FuncID = Integer

data Function i o = Function FuncID 
   deriving Show    
             
----------------------------------------------------------------------------
data a :- b = a :- b 
infixr :- 

class ArgList a where 
  argList :: a -> [Expr] 

instance ArgList () where -- needed = 
  argList () = [] 

instance ArgList (Exp a) where 
  argList (E a) = [a] 


instance ArgList a => ArgList (Exp b :- a) where 
  argList (E b :- a) = b : argList a 

----------------------------------------------------------------------------
-- Types for the i and o 


-- create specific base cases.. need something special for scalars. 
-- 
type family FunOut b 
--type instance FunOut (Exp b) = b 
--type instance FunOut (Exp b, Exp c) = b :- c 
--type instance FunOut (Exp b, Exp c, Exp d) = b :- c :- d 
--type instance FunOut (a -> b) = FunOut b 

type family FunIn a b
--type instance FunIn (Exp a) (Exp b) = a 
--type instance FunIn () (Exp b) = () 
--type instance FunIn (Exp a) (b -> c) = a :- (FunIn b c)

