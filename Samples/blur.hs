import Intel.ArBB 
import Intel.ArBB.Util.Image

import qualified Data.Vector.Storable as V 
import Prelude as P hiding (length,map)
import Data.Word


import System.IO
import Foreign hiding (new)


blur :: Exp (DVector Dim2 Word8) 
        -> Exp (DVector Dim2 Word8) 
blur image = vec2DToWord8 (res `div`  all16) 
  where 
    all16 = constVector2D (getNRows image) (getNCols image) 16
    res = mapStencil (Stencil [1,2,1
                              ,2,4,2   
                              ,1,2,1] (Z:.3:.3)) image'      
    image' = vec2DToWord32 image        

testConv =  
    withArBB $ 
      do 
        f <- capture blur 
     
        img <- liftIO$  loadRAW_Gray "window.raw" 256 256 
   
        v1 <- copyIn img 
        r1 <- new (Z:.256:.256) 0

        execute f v1 r1
              
        r <- copyOut r1

        liftIO $ saveRAW_Gray "convout.raw" r  
             

main = testConv