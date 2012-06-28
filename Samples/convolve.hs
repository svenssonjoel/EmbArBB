import Intel.ArBB 
import Intel.ArBB.Util.Image

import qualified Data.Vector.Storable as V 
import Prelude as P hiding (length,map)
import Data.Word


import System.IO
import Foreign hiding (new)


-- TODO: Remember that there is cheating going on in 
--       the code generation parts to make this work (typecheck)
conv :: Exp (DVector Dim2 Word8) 
        -> Exp (DVector Dim2 Word8) 
conv image = vec2DToWord8 (blur `div`  all16) 
  where 
    all16 = constVector2D 16 (getNRows image) (getNCols image)
    blur = mapStencil (Stencil [1,2,1
                               ,2,4,2   
                               ,1,2,1] (Z:.3:.3)) image'      
    image' = vec2DToWord32 image        

testSobel =  
    withArBB $ 
      do 
        f <- capture conv 
     
        img <- liftIO$  loadRAW_Gray "window.raw" 256 256 
   
        v1 <- copyIn img 
        r1 <- new (Z:.256:.256) 0

        execute f v1 r1
              
        r <- copyOut r1

        liftIO $ saveRAW_Gray "convout.raw" r  
             

main = testSobel