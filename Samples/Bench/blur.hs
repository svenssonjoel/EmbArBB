import Intel.ArBB 
import Intel.ArBB.Util.Image

import qualified Data.Vector.Storable as V 
import Prelude as P hiding (length,map)
import Data.Word

import System.Time
import Text.Printf
import System.Environment

import System.IO
import Foreign hiding (new)


blur :: Exp (DVector Dim3 Word8) 
        -> Exp (DVector Dim3 Word8) 
blur image = image3DToWord8 (blurred / all159) 
  where 

              -- here r c p 
    all159 = constVector3D r c p (159 :: Exp Double)
    (r,c,p) = (getNRows image,
               getNCols image,
               getNPages image)
                                        
               -- Here order is p r c
    blurred = setRegularNesting3D p r c 
                ( flatten red'  `cat`                      
                  flatten green' `cat` 
                  flatten blue' ) 
              

                                             
    red'   = blur' red
    green' = blur' green
    blue'  = blur' blue 

    image' = image3DToDouble image 

    red    = extractPage 0 image' 
    green  = extractPage 1 image' 
    blue   = extractPage 2 image' 

    blur' i = mapStencil (Stencil [ 2, 4, 5, 4, 2
                                  , 4, 9,12, 9, 4   
                                  , 5,12,15,12, 5
                                  , 4, 9,12, 9, 4
                                  , 2, 4, 5, 4, 2]  (Z:.5:.5)) i      
          




testConv =  
    withArBB $ 
      do 
        f <- capture blur 
     
        img <- liftIO$  loadBMP_RGB "cat1024.bmp"
   
        v1 <- copyIn img 
        r1 <- new (Z:.1024:.1024:.3) 0
        
        -- warmup
        execute f v1 r1
        finish 
                
        t1 <- liftIO getClockTime 
        execute f v1 r1
        finish
        t2 <- liftIO getClockTime 
        
              
        r <- copyOut r1

        liftIO $ saveBMP_RGB "blurcat.bmp" r  
        
        liftIO$ putStrLn "Done!"    
        liftIO $ printf "%f\n"  (diffms (diffClockTimes t2 t1))
    

main = testConv

picoToMs p = (fromIntegral p) * 1E-9

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