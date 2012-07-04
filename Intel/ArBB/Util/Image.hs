
{- Joel Svensson 2012 -} 

module Intel.ArBB.Util.Image ( loadBMP_RGB
                             , loadRAW_RGB 
                             , loadRAW_Gray 
                             , saveBMP_Gray
                             , saveBMP_RGB
                             , saveRAW_Gray 
                             , toGrayNaive 
                             , toGray
                             , grayToFloat
                             , floatToGray) where 


import Intel.ArBB.Vector 
import Intel.ArBB.Language as Lang
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
        (Right (ImageRGB8 img)) -> return $ imageToDVectorRGB img 


imageToDVectorRGB img = 
    let (Image _ _ r) = extractComponent 0 img 
        (Image _ _ g) = extractComponent 1 img 
        (Image w h b) = extractComponent 2 img
    in DVector (r V.++ g V.++ b)  (fromDim (Dim [3,w,h]))

loadRAW_RGB :: FilePath -> Int -> Int -> IO (DVector Dim3 Word8) 
loadRAW_RGB fp w h = 
    withFile fp ReadMode $ \handle -> do 
      ptr <- mallocBytes (3*w*h) 
      hGetBuf handle ptr (3*w*h) 
      --TODO: Stop going through lists 
      dat <- peekArray (3*w*h) ptr 
      let img = Image w h (V.fromList dat) :: Image PixelRGB8 
          dv  = imageToDVectorRGB img
      free ptr 
      return $ dv 

loadRAW_Gray :: FilePath -> Int -> Int -> IO (DVector Dim2 Word8) 
loadRAW_Gray fp w h =  withFile fp ReadMode $ \handle -> do 
      ptr <- mallocBytes (w*h) 
      hGetBuf handle ptr (w*h) 
      --TODO: Stop going through lists 
      dat <- peekArray (w*h) ptr 
      let vec = (V.fromList dat)
      free ptr 
      return $ DVector vec (fromDim (Dim [w,h])) 

saveBMP_Gray :: FilePath -> DVector Dim2 Word8 -> IO () 
saveBMP_Gray fp img = 
    writeBitmap fp image

      where 
        image = Image w h (dVectorData img) :: Image Pixel8
        -- Is this the right order again?! 
        Dim [w,h] = toDim$ dVectorShape img


combineImage :: Image Pixel8 
             -> Image Pixel8 
             -> Image Pixel8              
             -> Image PixelRGB8
combineImage img1 img2 img3 =
    generateImage combiner (imageWidth img1) (imageHeight img1)
        where combiner x y = PixelRGB8 (pixelAt img1 x y)
                                       (pixelAt img2 x y)
                                       (pixelAt img3 x y)

saveBMP_RGB :: FilePath -> DVector Dim3 Word8 -> IO () 
saveBMP_RGB fp img = 
    writeBitmap fp image 
        where 
          Dim [p,r,c] = toDim (dVectorShape img) 
          dat = dVectorData img 
          n = r * c
          image = combineImage (Image c r (V.slice 0 n dat)) 
                               (Image c r (V.slice n n dat)) 
                               (Image c r (V.slice (2*n) n dat))

saveRAW_Gray :: FilePath -> DVector Dim2 Word8 -> IO () 
saveRAW_Gray fp img = 
    withFile fp WriteMode $ \handle -> do 
      let (fptr,_) = V.unsafeToForeignPtr0 (dVectorData img) 
          ptr      = unsafeForeignPtrToPtr fptr
      hPutBuf handle ptr (w*h)
    where 
      Dim [w,h] = toDim$ dVectorShape img


----------------------------------------------------------------------------
-- EmbArBB image manip. functions

toGrayNaive :: Exp (DVector Dim3 Word8) -> Exp (DVector Dim2 Word8)  
toGrayNaive v = (addReduce pages v)  `div` ss'
    where 
      h = getNRows v
      w = getNCols v 
      ss = constVector w 3
      ss' = repeatRow ss h 
      

-- Converts to grayscale and corrects for perception 
-- of intensities of different color components 
toGray :: Exp (DVector Dim3 Word8) -> Exp (DVector Dim2 Word8) 
toGray v = vec2DToWord8 $ (redPlane * constVector2D w h wr) + 
                          (greenPlane * constVector2D w h wg) + 
                          (bluePlane * constVector2D w h wb) 
  where
    w = getNRows v
    h = getNCols v
    fv = vec3DToFloat v
    redPlane   = (extractPage fv 0) 
    greenPlane = (extractPage fv 1) 
    bluePlane  = (extractPage fv 2) 
    wr = mkFloat 0.2989 
    wg = mkFloat 0.5870 
    wb = mkFloat 0.1140 


-- convert grayscale 0 to 255 to float 0 to 1 
grayToFloat :: Exp (DVector Dim2 Word8) -> Exp (DVector Dim2 Float) 
grayToFloat v = fv / all255
    where 
      all255 = constVector2D r c 255
      fv     = vec2DToFloat v
      r = getNRows v 
      c = getNCols v

floatToGray :: Exp (DVector Dim2 Float) -> Exp (DVector Dim2 Word8) 
floatToGray v = (vec2DToWord8 fv)
    where 
      all255 = constVector2D r c 255
      fv = (Lang.map clamp v) * all255
      r = getNRows v
      c = getNCols v 
      
      

-- TODO: Improve on this. At least it definitely should not be defined here. 
mkFloat :: Float -> Exp Float 
mkFloat f = E $ Lit (LitFloat f ) 


clamp :: Exp Float -> Exp Float
-- Should be clamp, right ? 
clamp x = max 0 (min x 1)  