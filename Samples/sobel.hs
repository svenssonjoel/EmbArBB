import Intel.ArBB 
import Intel.ArBB.Util.Image

import qualified Data.Vector.Storable as V 
import Prelude as P hiding (length,map)
import Data.Word


import System.IO
import Foreign hiding (new)

{- 
      | -1  0  1 |
Gx =  | -2  0  2 | 
      | -1  0  1 | 

      | -1 -2 -1 | 
Gy =  |  0  0  0 | 
      |  1  2  1 | 

-} 

-- Haskell lists and list comprehensions used as a tool
s1, s2 :: [(Exp ISize,Exp ISize)]
s1 = [(1,1),(0,1),(-1,1),(1,-1),(0,-1),(-1,-1)]
s2 = [(1,1),(1,0),(1,-1),(-1,1),(-1,0),(-1,-1)]

coeffs :: [Exp Float] 
coeffs = [-1,-2,-1,1,2,1] 


gx :: Exp Word8 -> Exp Float  
gx x = foldl (+) 0 
     $ P.zipWith (*) [toFloat (getNeighbor2D x a b) / 255 
                    | (a,b) <- s1] coeffs 

gy :: Exp Word8 -> Exp Float 
gy x = foldl (+) 0 
     $ P.zipWith (*) [toFloat (getNeighbor2D x a b) / 255
                   | (a,b) <- s2] coeffs 

convertToWord8 :: Exp Float -> Exp Word8 
convertToWord8 x = toWord8 $ (clamp x)  * 255

clamp :: Exp Float -> Exp Float
-- Should be clamp, right ? 
clamp x = max 0 (min x 1)  
          

-- 8 bit per pixel greyscale image will be processed. 
kernel :: Exp Word8 -> Exp Word8 
kernel x = convertToWord8 $ body x
  where 
    body x = sqrt (x' * x' + y' * y') 
     where 
       x' = gx x 
       y' = gy x
        
       
sobel :: Exp (DVector Dim2 Word8) 
         -> Exp (DVector Dim2 Word8) 
sobel image = map kernel image  


testSobel =  
    withArBB $ 
      do 
        f <- capture sobel 
     
        img <- liftIO$  loadRAW_Gray "window.raw" 256 256 
   
        v1 <- copyIn img 
        r1 <- new (Z:.256:.256) 0

        execute f v1 r1
              
        r <- copyOut r1

        liftIO $ saveRAW_Gray "sobout.raw" r  
             

main = testSobel