import Intel.ArBB 
import Intel.ArBB.Util.Image
import qualified Intel.ArbbVM as VM

import qualified Data.Vector.Storable as V 
import qualified Prelude as P 
import           Prelude as P hiding (map,zipWith)
import Data.Word


import System.IO
import System.Time
import Text.Printf
import System.Environment

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


gx' :: Exp Float -> Exp Float  
gx' x = foldl (+) 0 
     $ P.zipWith (*) [getNeighbor2D x a b
                     | (a,b) <- s1] coeffs 

gy' :: Exp Float -> Exp Float 
gy' x = foldl (+) 0 
     $ P.zipWith (*) [getNeighbor2D x a b
                     | (a,b) <- s2] coeffs 


gx :: Exp (DVector Dim2 Float) -> Exp (DVector Dim2 Float)  
gx  = mapStencil (Stencil [-1,0,1
                          ,-2,0,2
                          ,-1,0,1] (Z:.3:.3)) 

gy :: Exp (DVector Dim2 Float) -> Exp (DVector Dim2 Float) 
gy = mapStencil (Stencil [ 1, 2, 1
                         , 0, 0, 0
                         ,-1,-2,-1] (Z:.3:.3))




test :: Exp (DVector Dim2 Float) -> 
        (Exp (DVector Dim2 Float), Exp (DVector Dim2 Float))
        
test v = (gx v,gy v)





--convertToWord8 :: Exp Float -> Exp Word8 
--convertToWord8 x = toWord8 $ (clamp x)  * 255

--clamp :: Exp Float -> Exp Float
-- Should be clamp, right ? 
--clamp x = max 0 (min x 1)  
          

-- 8 bit per pixel greyscale image will be processed. 
--kernel :: Exp Word8 -> Exp Word8 
--kernel x = convertToWord8 $ body x
--  where 
--    body x = sqrt (x' * x' + y' * y') 
--     where 
 --      x' = gx x 
--       y' = gy x
        
magnitude :: Exp Float -> Exp Float -> Exp Float
magnitude x y = sqrt (x * x + y * y) 

runMag :: Exp (DVector Dim2 Float) -> Exp (DVector Dim2 Float) -> 
          Exp (DVector Dim2 Float) 
runMag v1 v2 =  zipWith magnitude v1 v2
       
--sobel :: Exp (DVector Dim2 Word8) 
--         -> Exp (DVector Dim2 Word8) 
--sobel image = map kernel image  


testSobel nIters infile outfile =  
    withArBB $ 
      do 
        -- f <- capture sobel 
             
        test <- capture test
        runMag <- capture runMag
             
        convertGray <- capture toGray 
                       
        convertF <- capture grayToFloat
        convertG <- capture floatToGray
        
        img <- liftIO$  loadBMP_RGB infile 
       
        let (Dim [3,r,c]) = toDim$ dVectorShape img

        imgColor <- copyIn img 
        imgGray  <- new (Z:.c:.r) 0
        imgFloat <- new (Z:.c:.r) 0 
        imgF1    <- new (Z:.c:.r) 0 
        imgF2    <- new (Z:.c:.r) 0 


        execute convertGray imgColor imgGray 
        execute convertF imgGray imgFloat 
        
        -- warmup
        execute test imgFloat (imgF1 :- imgF2) 
        finish

        t1 <- liftIO getClockTime        
        execute test imgFloat (imgF1 :- imgF2) 
        finish
        t2 <- liftIO getClockTime      

        -- Warmup
        --execute runMag (imgF1 :- imgF2) (imgFloat)
        --finish

        --t1 <- liftIO getClockTime        
        --loop nIters $ 
        --    do 
        execute runMag (imgF1 :- imgF2) (imgFloat)
        --       finish 
        --t2 <- liftIO getClockTime      


        execute convertG imgFloat imgGray
        finish 

        r <- copyOut imgGray

        liftIO $ saveBMP_Gray outfile r  
        liftIO $ printf "%f\n"  (diffms (diffClockTimes t2 t1))  

mb a = 1024*1024*a
main = 
    do 
      VM.setHeapSize (mb 1024) (mb (8192*2)) 
    
      args <- getArgs 
      case args of 
        [a,b] -> 
            let iters = read a :: Int 
            in
              do 
                -- putStrLn $ "running " ++ show iters ++ " times"
                testSobel iters b "sobout.bmp" 
        _ -> error "incorrects args" 

loop 0 action = return () 
loop n action = 
    do 
      action 
      loop (n-1) action 

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

