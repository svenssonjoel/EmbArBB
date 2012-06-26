{-# LANGUAGE ScopedTypeVariables #-} 

{- 2012 Joel Svensson -} 

import Intel.ArBB 
import Intel.ArBB.Util.Image

import qualified Data.Vector.Storable as V 

import Prelude hiding (length)

import Data.Int
import Data.Word

import System.IO
import Foreign hiding (new)

-- NOTE: The c++ version uses some kind of trick where a 1 is turned into an array of suitable size 
--       add_merge(1,input,256) ... 


histogram :: Exp (DVector Dim2 Word8)  -> Exp (Vector Word32)
histogram input = addMerge cv (vecToUSize flat) 256
    where
      flat = flatten input
      cv = constVector 1 (r * c) 
      r  = getNRows input 
      c  = getNCols input
      

-- a little less a hack..
testHist2 =
  do  
    withArBB $ 
      do 
        f <- capture histogram 
      
        raw <- liftIO$ loadRAW_Gray "window.raw" 256 256 
             
        v1 <- copyIn raw  
        r1 <- new (Z:.256) 0  

        execute f v1 r1
              
        (DVector r _) <- copyOut r1 
        
        let r' = V.toList r

        pt2 <- liftIO$ mallocBytes (256 * 256 * 3) 
        liftIO $ pokeArray pt2 (replicate (256*256*3) 0) 
        let m = maximum r' 

       
        liftIO $ sequence_ [lineH pt2 i 0 (scale 256 m  (r' !! i)) | i <- [0..255]]
        liftIO $ withBinaryFile "out.raw" WriteMode $ \ handle -> 
            hPutBuf handle pt2 (256 * 256 * 3)   
        liftIO $ putStrLn "Result is stored in: out.raw" 


scale w m x = round (fromIntegral w / fromIntegral m * fromIntegral x)


lineH pt2 y x1 x2 =
    sequence_ 
    $ concat [[pokeElemOff pt2 ((y*256+x)*3) (255 :: Word8),
               pokeElemOff pt2 ((y*256+x)*3+1) (255 :: Word8),
               pokeElemOff pt2 ((y*256+x)*3+2) (255 :: Word8)] | x <- [x1..x2]]


main = testHist2