import Intel.ArBB 
import Intel.ArBB.Util.Image

import qualified Data.Vector.Storable as V 
import Prelude as P hiding (length,map)
import Data.Word
import qualified Data.List as L

import System.Time
import Text.Printf
import System.Environment

import System.IO
import Foreign hiding (new)

smallDiv =  16
stencilSmall  = Stencil [ 1, 2, 1
                        , 2, 4, 2
                        , 1, 2, 1] (Z:.3:.3)

mediumDiv = 159
stencilMedium = Stencil [ 2, 4, 5, 4, 2
                        , 4, 9,12, 9, 4   
                        , 5,12,15,12, 5
                        , 4, 9,12, 9, 4
                        , 2, 4, 5, 4, 2]  (Z:.5:.5)
largeDiv = 140
stencilLarge = Stencil [ 1, 1, 2, 2, 2, 1, 1 
                       , 1, 2, 2, 4, 2, 2, 1 
                       , 2, 2, 4, 8, 4, 2, 2 
                       , 2, 4, 8,16, 8, 4, 2 
                       , 2, 2, 4, 8, 4, 2, 2 
                       , 1, 2, 2, 4, 2, 2, 1 
                       , 1, 1, 2, 2, 2, 1, 1] (Z:.7:.7)


blur :: (Stencil (Exp Double) (Dim2), Exp Double)
     -> Exp (DVector Dim3 Word8) 
     -> Exp (DVector Dim3 Word8) 
blur (st,divisor) image = image3DToWord8 (blurred / alld) 
  where 

              -- here r c p 
    alld = constVector3D r c p (divisor)
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

    blur' i = mapStencil st i      
          


testBlur eS infile outfile =  
    withArBB $ 
      do 
        f <- capture (blur eS)  --(stencilLarge,largeDiv))  
     
        img <- liftIO$  loadBMP_RGB infile

        let Dim [p,c,r] = toDim $ dVectorShape img
   
        v1 <- copyIn img 
        r1 <- new (Z:.r:.c:.p) 0
        
        -- warmup
        execute f v1 r1
        finish 
                
        t1 <- liftIO getClockTime 
        execute f v1 r1
        finish
        t2 <- liftIO getClockTime 
        
              
        r <- copyOut r1

        liftIO $ saveBMP_RGB outfile r  
        
        -- liftIO$ putStrLn "Done!"    
        liftIO $ printf "%f\n"  (diffms (diffClockTimes t2 t1))
    


eSize = [ (stencilSmall,smallDiv)
        , (stencilMedium,mediumDiv)
        , (stencilLarge,largeDiv)]

main = 
    do 
      [s,infile,outfile] <- getArgs 
      let Just i = L.elemIndex s ["small","medium","large"]
      testBlur (eSize L.!! i) infile outfile 

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