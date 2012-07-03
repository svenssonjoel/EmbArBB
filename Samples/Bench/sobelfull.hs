import Intel.ArBB 
import Intel.ArBB.Util.Image
import qualified Intel.ArbbVM as VM

import qualified Data.Vector.Storable as V 
import Prelude as P hiding (length,map)
import Data.Word

import System.IO
import System.Time
import Text.Printf
import System.Environment

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


testSobel infile outfile =  
    withArBB $ 
      do 
        f <- capture sobel 
        
        tg <- capture toGray
     
        img <- liftIO$  loadBMP_RGB infile 

        let (Dim [3,r,c]) = toDim$ dVectorShape img
   
        v1 <- copyIn img 
        r1 <- new (Z:.c:.r) 0
        r2 <- new (Z:.c:.r) 0
              
        execute tg v1 r1
        
        -- warm up
        execute f r1 r2
        finish
        
        t1 <- liftIO$ getClockTime 
        execute f r1 r2
        finish
        t2 <- liftIO$ getClockTime 

        r <- copyOut r2

        liftIO $ saveBMP_Gray outfile r  
        liftIO $ printf "%f\n"  (diffms (diffClockTimes t2 t1))  
             
mb a = 1024*1024*a
main = 
    do 
      VM.setHeapSize (mb 1024) (mb 2048) 
    
      args <- getArgs 
      case args of 
        [a] -> testSobel a "sobout.bmp" 
        _ -> error "incorrects args" 

diffms :: TimeDiff -> Float
diffms diff | tdYear diff == 0 && 
              tdMonth diff == 0 && 
              tdDay diff == 0 && 
              tdMin diff == 0 && 
              tdHour diff == 0  =  (fromIntegral ps) * 1E-9  + 
                                   (fromIntegral sec) * 1000                
             where 
               ps  = tdPicosec diff 
               sec = tdSec diff
