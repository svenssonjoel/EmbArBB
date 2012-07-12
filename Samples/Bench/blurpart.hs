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

smallDiv =  16 :: Exp Double
stencilSmall  = Stencil [ 1, 2, 1
                        , 2, 4, 2
                        , 1, 2, 1] (Z:.3:.3)

mediumDiv = 159 :: Exp Double 
stencilMedium = Stencil [ 2, 4, 5, 4, 2
                        , 4, 9,12, 9, 4   
                        , 5,12,15,12, 5
                        , 4, 9,12, 9, 4
                        , 2, 4, 5, 4, 2]  (Z:.5:.5)
largeDiv = 140 :: Exp Double 
stencilLarge = Stencil [ 1, 1, 2, 2, 2, 1, 1 
                       , 1, 2, 2, 4, 2, 2, 1 
                       , 2, 2, 4, 8, 4, 2, 2 
                       , 2, 4, 8,16, 8, 4, 2 
                       , 2, 2, 4, 8, 4, 2, 2 
                       , 1, 2, 2, 4, 2, 2, 1 
                       , 1, 1, 2, 2, 2, 1, 1] (Z:.7:.7)


blur :: (Stencil (Exp Double) (Dim2), Exp Double)
     -> Exp (DVector Dim2 Double) 
     -> Exp (DVector Dim2 Double) 
blur (st,divisor) image = (image' / alld) 
  where 
              -- here r c p 
    alld = constVector2D r c (divisor)
    (r,c) = (getNRows image,
             getNCols image)
                                                          
    image'   = blur' image

    blur' i = mapStencil st i      
          

decompose :: Exp (DVector Dim3 a) -> 
             (Exp (DVector Dim2 a), 
              Exp (DVector Dim2 a), 
              Exp (DVector Dim2 a)) 
decompose v = (extractPage 0 v, 
               extractPage 1 v, 
               extractPage 2 v) 
                        


testBlur eS infile outfile =  
    withArBB $ 
      do 
        f <- capture (blur eS)  --(stencilLarge,largeDiv))  
             
        dc <- capture decompose
        td <- capture image3DToDouble
     --    tw <- capture image3DToWord8
     
        img <- liftIO$  loadBMP_RGB infile

        let Dim [p,c,r] = toDim $ dVectorShape img
   
        v1 <- copyIn img 
        --r1 <- new (Z:.r:.c) 0
        rr <- new (Z:.r:.c) 0
        rg <- new (Z:.r:.c) 0
        rb <- new (Z:.r:.c) 0
        
        fimg <- new (Z:.r:.c:.p) 0
              
        red <- new (Z:.r:.c) 0
        green <- new (Z:.r:.c) 0
        blue <- new (Z:.r:.c) 0

        execute td v1 fimg

        execute dc fimg (red :- green :- blue) 

        
              
        -- warmup
        execute f red rr
        finish 
                
        t1 <- liftIO getClockTime 
        execute f red rr
        execute f green rg
        execute f blue rb 
        finish
        t2 <- liftIO getClockTime 
        
              
        --r <- copyOut rr

        --liftIO $ saveBMP_Gray outfile r  
        
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