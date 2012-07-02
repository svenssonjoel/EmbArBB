
{-# LANGUAGE ScopedTypeVariables,
             TypeOperators #-} 

{- 2012 Joel Svensson -} 

module Test where 


import qualified Intel.ArbbVM as VM
import qualified Intel.ArbbVM.Convenience as VM


import Intel.ArBB.Vector
import Intel.ArBB.Data.Int
import Intel.ArBB.Data.Boolean


import Intel.ArBB
import Intel.ArBB.Language as Lang
import Intel.ArBB.Backend.ArBB
import Intel.ArBB.Util.Image 

import qualified Data.Vector.Storable as V 

import Foreign.Marshal.Array
import qualified Foreign.Marshal.Utils as Utils
import Foreign.Ptr

import Data.Int 
import Data.Word
import Data.IORef

import qualified Data.Map as Map
import Control.Monad.State hiding (liftIO)

import Prelude as P

test1 = 
    withArBB $ do 
      f <- capture redChan
      g <- capture greenChan
      h <- capture blueChan
         
      tg <- capture toGray -- Naive

      str <- serialize f 
      liftIO$ putStrLn str

      img <- liftIO$ loadBMP_RGB "image1.bmp"
           
      bmp' <- copyIn img
             
      r1 <- new (Z:.256:.256) 0 
      r2 <- new (Z:.256:.256) 0 
      r3 <- new (Z:.256:.256) 0
      r4 <- new (Z:.256:.256) 0 

      execute f bmp' r1 
      execute g bmp' r2 
      execute h bmp' r3 
      execute tg bmp' r4

      r1' <- copyOut r1
      r2' <- copyOut r2
      r3' <- copyOut r3
      r4' <- copyOut r4

      --liftIO$ putStrLn $ show r1'
      --liftIO$ putStrLn $ show r2'
      --liftIO$ putStrLn $ show r3'
      liftIO$ putStrLn $ show r4'
      liftIO$ putStrLn "Done!"
    where 
      redChan :: Exp (DVector Dim3 Word8) -> Exp (DVector Dim2 Word8)
      redChan v = extractPage v 0

      greenChan :: Exp (DVector Dim3 Word8) -> Exp (DVector Dim2 Word8)
      greenChan v = extractPage v 1

      blueChan :: Exp (DVector Dim3 Word8) -> Exp (DVector Dim2 Word8)
      blueChan v = extractPage v 2


testToGray= 
    withArBB $ do 
      tg <- capture toGray -- Naive

      str <- serialize tg
      liftIO$ putStrLn str

      img <- liftIO$ loadBMP_RGB "image2.bmp"
           
      bmp' <- copyIn img
             
      r <- new (Z:.256:.256) 0 

      execute tg bmp' r
     
      r' <- copyOut r

      liftIO$ saveBMP_Gray "out.bmp" r'
--       liftIO$ putStrLn $ show r'
      liftIO$ putStrLn "Done!"


testWriteBmp = 
    do 
      img <- loadBMP_RGB "image2.bmp"
      saveBMP_RGB "image3.bmp" img


segPrefixSum :: Exp (DVector Dim1 Float) 
              -> Exp (DVector Dim1 USize) 
               -> Exp (DVector Dim1 Float)
segPrefixSum v1 v2  = flattenSeg $ addScanSeg (applyNesting v1 v2 1)



testSegPrefixSum =
  withArBB $
    do
     f <- capture segPrefixSum

     let v1 = V.fromList [1,3,5,7,9,11,13,15]
         v2 = V.fromList [4,4]

     v3 <- copyIn $ mkDVector v1 (Z :. 8)
     v4 <- copyIn $ mkDVector v2 (Z :. 2)

     r1 <- new (Z :. 8) 0

     execute f (v3 :- v4) r1

     r <- copyOut r1

     liftIO$ putStrLn$ show r


testBool = 
    withArBB $ 
    do 
      f <- capture bt

      let v1 = V.fromList [B True,B True,B True,B True,B True,B True,B True,B True] 
      
      v <- copyIn $ mkDVector v1 (Z:.8) 
      r1 <- new (Z:.8) (B False)
      
      execute f v r1 
           
      r <- copyOut r1

      liftIO$ putStrLn$ show r 
    where
      bt :: Exp (DVector Dim1 Boolean) -> Exp (DVector Dim1 Boolean)
      bt v = andScan v 0 0 