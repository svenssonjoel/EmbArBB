import Intel.ArBB 

import qualified Data.Vector.Storable as V 
import Prelude hiding (length,map)
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
     $ zipWith (*) [toFloat (getNeighbor2D x a b) / 255 
                    | (a,b) <- s1] coeffs 


gy :: Exp Word8 -> Exp Float 
gy x = foldl (+) 0 
     $ zipWith (*) [toFloat (getNeighbor2D x a b) / 255
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
       -- y'' = y' * y'
       -- x'' = x' * x' 
       x' = gx x 
       y' = gy x
        
       
sobel :: Exp (DVector Dim2 Word8) 
         -> Exp (DVector Dim2 Word8) 
sobel image = map kernel image  


-- Very much a hack.. 
testSobel =
  do  
    ptr <- mallocBytes (256 * 256) 
    withBinaryFile "window.raw" ReadMode $ \ handle -> 
      hGetBuf handle ptr (256 * 256) 
    ls <- peekArray (256 * 256) ptr

    -- TODO: Stop going through lists.
    withArBB $ 
      do 
        f    <- capture sobel 
        
        v1 <- copyIn (V.fromList ls) (Z:.256:.256)
        r1 <- new (Z:.256:.256) 0

        execute f v1 r1
              
        r <- copyOut r1

        let r' = V.toList r

        pt2 <- liftIO$ mallocBytes (256 * 256) 
        liftIO $ pokeArray pt2 r'
       
       
        liftIO $ withBinaryFile "sobout.raw" WriteMode $ \ handle -> 
            hPutBuf handle pt2 (256 * 256)   
        liftIO $ putStrLn "Result is stored in: sobout.raw" 


main = testSobel