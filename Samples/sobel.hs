import Intel.ArBB 

import qualified Data.Vector.Storable as V 
import Prelude hiding (length,map)
import Data.Word

{- 
      | -1  0  1 |
Gx =  | -2  0  2 | 
      | -1  0  1 | 

      | -1 -2 -1 | 
Gy =  |  0  0  0 | 
      |  1  2  1 | 

-} 

gx :: Exp Float -> Exp Float  
gx x = (-p0) + (-2) * p1 + (-p2) +  
         p3  +   2  * p4 +   p5 
  where 
    p0 = getNeighbor2D x   1  1 
    p1 = getNeighbor2D x   0  1
    p2 = getNeighbor2D x (-1) 1 
    
    p3 = getNeighbor2D x   1  (-1)
    p4 = getNeighbor2D x   0  (-1)
    p5 = getNeighbor2D x (-1) (-1)

gy :: Exp Float -> Exp Float 
gy x = (-p0) + (-2) * p1 + (-p2) +  
         p3  +   2  * p4 +   p3 
  where 
    p0 = getNeighbor2D x  1   1
    p1 = getNeighbor2D x  1   0
    p2 = getNeighbor2D x  1 (-1) 
    
    p3 = getNeighbor2D x (-1)   1 
    p4 = getNeighbor2D x (-1)   0
    p5 = getNeighbor2D x (-1) (-1)

convertToFloat :: Exp Word8 -> Exp Float 
convertToFloat x = (toFloat x) / 255

convertToWord8 :: Exp Float -> Exp Word8 
convertToWord8 x = toWord8 $ (clamp x)  * 255

clamp :: Exp Float -> Exp Float
clamp x = ifThenElse (1 <* x) 1 x
          

-- 8 bit per pixel greyscale image will be processed. 
kernel :: Exp Word8 -> Exp Word8 
kernel x = convertToWord8 
         $ body 
         $ convertToFloat x
  where 
    body x = sqrt (x'' + y'') 
     where 
       y'' = y' * y'
       x'' = x' * x' 
       x' = gx x 
       y' = gy x
       
sobel :: Function (EIn (Exp Word8) (Exp Word8)) (EOut (Exp Word8)) -> Exp (DVector Dim2 Word8) -> Exp (DVector Dim2 Word8) 
sobel = error "hello" 

getCoord :: Exp Word32 -> Exp Word32
getCoord x = x + getNeighbor x 0 0 (-1)

mapper :: Function (EIn (Exp Word32) (Exp Word32)) (EOut (Exp Word32)) -> Exp (Vector Word32) -> Exp (Vector Word32) 
mapper f v = map f v 


testMap = 
  withArBB $ 
  do 
     f2 <- capture2 getCoord     

     strf <- serialize f2
     liftIO$ putStrLn strf


     f3 <- capture2 kernel     

     strf <- serialize f3
     liftIO$ putStrLn strf


     m2 <- capture2 (mapper f2) 

     strm <- serialize m2 
     liftIO$ putStrLn strm

     let x = Vector (V.fromList [1,2,3,4,5,6,7,8,9,10 :: Word32]) (One 10) 
     
     r2 <- liftIO$ new1D 10   

     execute2 m2 (x)  r2
              
     r' <- liftIO$ freeze r2
              
     liftIO$ putStrLn$ show r'


main = testMap