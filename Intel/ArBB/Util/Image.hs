
{- Joel Svensson 2012 -} 

module Intel.ArBB.Util.Image where 


import Intel.ArBB.Vector 

import Codec.Picture 
import Codec.Picture.Types
import qualified Data.Vector.Storable as V

import System.IO hiding (putStrLn) 
import Data.ByteString as BS hiding (putStrLn) 
import Data.Word


----------------------------------------------------------------------------
-- Load an RGB bitmap. 

-- TODO: make sure that the Red, Green and Blue channels are 
--       distributed correctly over the pages of the DVector. 
loadBMP_RGB :: FilePath -> IO (DVector Dim3 Word8)
loadBMP_RGB fp = 
    withFile fp ReadMode $ \handle -> do 
      bs <- BS.hGetContents handle 
      case decodeBitmap bs of 
        (Left str) -> error str 
        (Right (ImageRGB8 (Image w h img))) ->
              return $ DVector img (Dim [w,h,3])


loadBMP_DBG fp = 
    withFile fp ReadMode $ \handle -> do 
      bs <- BS.hGetContents handle 
      case decodeBitmap bs of 
        (Left str) -> error str 
        (Right (ImageRGB8 img)) ->
            do 
              let red   = extractComponent 0 img
                  green = extractComponent 1 img
                  blue  = extractComponent 2 img 
                  (Image _ _ vr)    = red 
                  (Image _ _ vg)    = green
                  (Image _ _ vb)    = blue 
              putStrLn $ show vr
              putStrLn $ show vg 
              putStrLn $ show vb
            




