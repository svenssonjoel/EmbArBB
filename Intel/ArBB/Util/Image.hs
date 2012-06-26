
{- Joel Svensson 2012 -} 

module Intel.ArBB.Util.Image where 


import Intel.ArBB.Vector 
import Intel.ArBB.Language
import Intel.ArBB.Syntax

import Codec.Picture 
import Codec.Picture.Types
import qualified Data.Vector.Storable as V

import System.IO hiding (putStrLn) 
import Data.ByteString as BS hiding (putStrLn,length) 
import Data.Word

import Prelude hiding (length)

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
        (Right (ImageRGB8 img)) ->
             do 
               let (Image _ _ r) = extractC 0 img 
                   (Image _ _ g) = extractC 1 img 
                   (Image w h b) = extractC 2 img
               return $ DVector (r V.++ g V.++ b)  (Dim [3,w,h])


extractC c (Image x y v)  = Image x y (V.ifilter (\i a -> i `mod` 3 == c) v) 

toGrayNaive :: Exp (DVector Dim3 Word8) -> Exp (DVector Dim2 Word8)  
toGrayNaive v = (addReduce v 2)  `div` ss'
    where 
      h = getNRows v
      w = getNCols v 
      ss = constVector 3 w 
      ss' = repeatRow ss h 
      

floatImage :: Exp (DVector Dim3 Word8) -> Exp (DVector Dim3 Float) 
floatImage v = undefined 

