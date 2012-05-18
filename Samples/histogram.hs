{-# LANGUAGE ScopedTypeVariables #-} 

{- 2012 Joel Svensson -} 

module Test where 

import Intel.ArBB 

import qualified Data.Vector.Storable as V 

import Prelude hiding (length)

import Data.Int
import Data.Word

import System.IO
import Foreign

-- TODO: The c++ version uses some kind of trick where a 1 is turned into an array of suitable size 
--       add_merge(1,input,256) ... 


histogram :: Exp (Vector Word8)  -> Exp (Vector Word32)
histogram input = addMerge cv (toUSize input) 256
    where
      cv = constVector 1 s
      s  = length input 
 

testHist = 
  withArBB $ 
  do 
     f <- capture2 histogram 
     let v1 = fromList ([0,0,0,4,4,5] ++ replicate 10000 1)
     
     r1 <- liftIO$ new1D 256   

     execute2 f v1 r1
              
     r <- liftIO$ freeze r1
              
     liftIO$ putStrLn$ show r



-- Very much a hack.. 
testHist2 =
  do  
    ptr <- mallocBytes (256 * 256) 
    withBinaryFile "Samples/window.raw" ReadMode $ \ handle -> 
      hGetBuf handle ptr (256 * 256) 
    ls <- peekArray (256 * 256) ptr

    -- TODO: Stop going through lists.
    withArBB $ 
      do 
        f <- capture2 histogram 
        let v1 = fromList ls 
     
        r1 <- liftIO$ new1D 256   

        execute2 f v1 r1
              
        (Vector r _) <- liftIO$ freeze r1
              
        -- liftIO$ putStrLn$ show r
        
        let r' = V.toList r
                    
        liftIO$ putStrLn$ show r'

        pt2 <- liftIO$ mallocBytes (256 * 256 * 3) 
        liftIO $ pokeArray pt2 (replicate (256*256*3) 0) 
        let m = maximum r' 

       
        liftIO $ sequence_ [lineH pt2 i 0 (scale 256 m  (r' !! i)) | i <- [0..255]]
        liftIO $ withBinaryFile "Samples/out.raw" WriteMode $ \ handle -> 
            hPutBuf handle pt2 (256 * 256 * 3)   


scale w m x = round (fromIntegral w / fromIntegral m * fromIntegral x)


lineH pt2 y x1 x2 =
    sequence_ 
    $ concat [[pokeElemOff pt2 ((y*256+x)*3) (255 :: Word8),
               pokeElemOff pt2 ((y*256+x)*3+1) (255 :: Word8),
               pokeElemOff pt2 ((y*256+x)*3+2) (255 :: Word8)] | x <- [x1..x2]]