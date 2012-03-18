module Intel.ArBB.Embeddable where 

import Intel.ArBB.Types 

class Embeddable a where 
  typeOf :: a -> Type 
  
  
