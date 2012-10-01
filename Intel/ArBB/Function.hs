{-# LANGUAGE TypeOperators, 
             FlexibleInstances,
             TypeFamilies,  
             CPP#-}


-- TODO: Move to backend ? 
module Intel.ArBB.Function where 

import Intel.ArBB.Syntax

import Intel.ArBB.Backend.Vector
import Intel.ArBB.Backend.Scalar
import Intel.ArBB.Vector 
import Intel.ArBB.Data.Int
import Intel.ArBB.Data.Boolean

import Data.Word
import Data.Int

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
type instance FunOut (Exp (DVector t a)) = BEDVector t a 
type instance FunOut (Exp (NVector a))   = BENVector a 

-- titta på Associerad typ i class (lösning på NVector input och output)
-- Om konstigt prata mer med Josef! 

#define ScalarOut(scal)                             \
  type instance FunOut (Exp (scal)) = BEScalar (scal)
  
ScalarOut(Int)
ScalarOut(Int8)
ScalarOut(Int16)
ScalarOut(Int32)
ScalarOut(Int64)
ScalarOut(Word)
ScalarOut(Word8)
ScalarOut(Word16)
ScalarOut(Word32)
ScalarOut(Word64)
ScalarOut(Float)
ScalarOut(Double)
ScalarOut(Boolean)

type instance FunOut (a,b) = FunOut a :- FunOut b 
type instance FunOut (a,b,c) = FunOut a :- FunOut b :- FunOut c 
type instance FunOut (a -> b) = FunOut b 

type family FunIn a b

type instance FunIn (Exp (DVector t1 a)) (Exp b) = BEDVector t1 a 
type instance FunIn (Exp (DVector t1 a)) (Exp b,Exp c) = BEDVector t1 a 
type instance FunIn (Exp (DVector t1 a)) (Exp b,Exp c,Exp d) = BEDVector t1 a 
type instance FunIn (Exp (DVector t1 a)) () = BEDVector t1 a 

type instance FunIn (Exp a) (b -> c) = FunIn (Exp a) () :- FunIn b c 

#define ScalarIn(scal,r)                                      \
  type instance FunIn (Exp (scal)) (r) = BEScalar (scal)
  

ScalarIn(Int,Exp b)
ScalarIn(Int8,Exp b)
ScalarIn(Int16,Exp b)
ScalarIn(Int32,Exp b)
ScalarIn(Int64,Exp b)
ScalarIn(Word,Exp b)
ScalarIn(Word8,Exp b)
ScalarIn(Word16,Exp b)
ScalarIn(Word32,Exp b)
ScalarIn(Word64,Exp b)
ScalarIn(Float,Exp b)
ScalarIn(Double,Exp b)
ScalarIn(Boolean,Exp b)
ScalarIn(USize, Exp b)
ScalarIn(ISize, Exp b) 

ScalarIn(Int,())
ScalarIn(Int8,())
ScalarIn(Int16,())
ScalarIn(Int32,())
ScalarIn(Int64,())
ScalarIn(Word,())
ScalarIn(Word8,())
ScalarIn(Word16,())
ScalarIn(Word32,())
ScalarIn(Word64,())
ScalarIn(Float,())
ScalarIn(Double,())
ScalarIn(Boolean,())
ScalarIn(USize, ())
ScalarIn(ISize, ()) 
