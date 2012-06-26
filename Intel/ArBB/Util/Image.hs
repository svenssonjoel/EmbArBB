
{- Joel Svensson 2012 -} 

module Intel.ArBB.Util.Image where 


import Intel.ArBB.Vector 
import Intel.ArBB.Language
import Intel.ArBB.Syntax
import Intel.ArBB.Literal

import Codec.Picture 
import Codec.Picture.Types
import qualified Data.Vector.Storable as V

import System.IO hiding (putStrLn)
import Foreign 
 
import Data.ByteString as BS hiding (putStrLn,length) 
import Data.Word

import Prelude hiding (length)

----------------------------------------------------------------------------
-- Load an RGB bitmap. 

-- Done: make sure that the Red, Green and Blue channels are 
--       distributed correctly over the pages of the DVector. 
--  * Works for now. 
loadBMP_RGB :: FilePath -> IO (DVector Dim3 Word8)
loadBMP_RGB fp = 
    withFile fp ReadMode $ \handle -> do 
      bs <- BS.hGetContents handle 
      case decodeBitmap bs of 
        (Left str) -> error str 
        (Right (ImageRGB8 img)) ->
             do 
               let (Image _ _ r) = extractC 0 img 
                   (Image _ _ g) = extractC 1 img 
                   (Image w h b) = extractC 2 img
               return $ DVector (r V.++ g V.++ b)  (Dim [3,w,h])

loadRAW_RGB :: FilePath -> Int -> Int -> IO (DVector Dim3 Word8) 
loadRAW_RGB fp w h = 
    withFile fp ReadMode $ \handle -> do 
      ptr <- mallocBytes (3*w*h) 
      --TODO: Stop going through lists 
      dat <- peekArray (3*w*h) ptr 
      let img = Image w h (V.fromList dat) :: Image PixelRGB8 
          (Image _ _ r) = extractC 0 img 
          (Image _ _ g) = extractC 1 img 
          (Image w h b) = extractC 2 img
      free ptr 
      return $ DVector (r V.++ g V.++ b) (Dim [3,w,h])

loadRAW_Gray :: FilePath -> Int -> Int -> IO (DVector Dim2 Word8) 
loadRAW_Gray fp w h =  withFile fp ReadMode $ \handle -> do 
      ptr <- mallocBytes (w*h) 
      --TODO: Stop going through lists 
      dat <- peekArray (w*h) ptr 
      let vec = (V.fromList dat)
      free ptr 
      return $ DVector vec (Dim [w,h]) 

extractC c (Image x y v)  = Image x y (V.ifilter (\i a -> i `mod` 3 == c) v) 

saveBMP_Gray :: FilePath -> DVector Dim2 Word8 -> IO () 
saveBMP_Gray fp img = 
    writeBitmap fp image

      where 
        image = Image w h (dVectorData img) :: Image Pixel8
        -- Is this the right order again?! 
        Dim [w,h] = dVectorShape img


toGrayNaive :: Exp (DVector Dim3 Word8) -> Exp (DVector Dim2 Word8)  
toGrayNaive v = (addReduce v 2)  `div` ss'
    where 
      h = getNRows v
      w = getNCols v 
      ss = constVector 3 w 
      ss' = repeatRow ss h 
      

-- Convers to grayscale and corrects for perception 
-- of intensities of different color components 
toGray :: Exp (DVector Dim3 Word8) -> Exp (DVector Dim2 Word8) 
toGray v = vec2DToWord8 $ ((redPlane * constVector2D wr w h) + 
                           (greenPlane * constVector2D wg w h) + 
                           (bluePlane * constVector2D wb w h) ) * scale
  where
    w = getNRows v
    h = getNCols v
    fv = vec3DToFloat v
    redPlane   = (extractPage fv 0) / scale
    greenPlane = (extractPage fv 1) / scale
    bluePlane  = (extractPage fv 2) / scale  
    scale      = constVector2D (mkFloat 255.0) w h 
    wr = mkFloat 0.2989 
    wg = mkFloat 0.5870 
    wb = mkFloat 0.1140 

-- TODO: Improve on this. At least it definitely should not be defined here. 
mkFloat :: Float -> Exp Float 
mkFloat f = E $ Lit (LitFloat f) 


