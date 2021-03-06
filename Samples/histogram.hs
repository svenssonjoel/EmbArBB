{-# LANGUAGE ScopedTypeVariables,
             BangPatterns #-} 

{- 2012 Joel Svensson -} 

import Intel.ArBB 
import Intel.ArBB.Util.Image

import qualified Data.Vector.Storable as V 

import Prelude hiding (length,map)

import Data.Int
import Data.Word

import System.IO
import Foreign hiding (new)

-- NOTE: The c++ version uses some kind of trick where a 1 is turned into an array of suitable size 
--       add_merge(1,input,256) ... 


histogram :: Exp (DVector Dim2 Word8)  -> Exp (Vector Word32)
histogram input = addMerge (vecToUSize flat) 256 cv
    where
      flat = flatten input
      cv = constVector (r * c) 1 
      r  = getNRows input 
      c  = getNCols input
      

-- Visiualise the histogram of an image.
histImage :: Exp (Vector Word32) -> Exp (DVector Dim2 Word8) 
histImage input = fst $ while cond body (cvn,0)    
    where 
      cond (img,i) = i <* n 
      body (img,i) = (replaceCol i col' img,i+1) 
          where 
            val = input ! i 
            col = extractCol i img
            col' = fill black 0 n col
            n = 255 - scale 255 m val  
                    
      n = length input 
      cv = constVector (n*n) white  
      cvn = setRegularNesting2D n n cv
      m   = index0 (maxReduce rows input)
      black = 0 
      white = 255

histCombined :: Exp (DVector Dim2 Word8) -> Exp (DVector Dim2 Word8) 
histCombined img = let a = histogram img
                   in histImage a 


-- a little less a hack..
testHist =
  do  
    withArBB $ 
      do 
        f <- capture histogram 
        g <- capture histImage
        tg <- capture toGray
      
        bmp <- liftIO$ loadBMP_RGB "cat.bmp" 
             
        v1 <- copyIn bmp
        gray <- new (Z:.256:.256) 0 
        r1 <- new (Z:.256) 0 
        r2 <- new (Z:.256:.256) 0 
      
        execute tg v1 gray
              
        execute f gray r1

        execute g r1 r2
              
        img <- copyOut r2
        dbg <- copyOut r1
        liftIO$ saveBMP_Gray "hist.bmp" img
        liftIO$ putStrLn $ show dbg

-- Breaks something
testHistCombined =
  do  
    withArBB $ 
      do 
        f <- capture histCombined
      
        tg <- capture toGray
      
        bmp <- liftIO$ loadBMP_RGB "cat.bmp" 
             
        v1 <- copyIn bmp
        gray <- new (Z:.256:.256) 0 
    
        r <- new (Z:.256:.256) 0 
      
        execute tg v1 gray
              
        execute f gray r
             
        img <- copyOut r
        
        liftIO$ saveBMP_Gray "hist.bmp" img

   

scale :: Exp Word32 -> Exp Word32 -> Exp Word32 -> Exp USize
scale n m x = toUsize $ ((toFloat n) / (toFloat m)) * (toFloat x)


main = testHist