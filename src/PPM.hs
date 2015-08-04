module PPM where

import System.Environment
import System.IO
import GHC.IO.Device
import qualified Data.Vector as V

----- Data Type -----
---------------------
-- Unsafe Vector data type for PPM Images
data PPM = PPM {pixels :: V.Vector (Int, Int, Int) , height :: Int, width :: Int, cap :: Int}
    deriving (Show)



----- Helper Functions -----
----------------------------
writeTup :: (Int, Int, Int) -> Handle -> IO ()
writeTup (r,g,b) h = hPutStrLn h $ (show r) ++ " " ++ (show g) ++ " " ++ (show b)

writeRow :: V.Vector (Int, Int, Int) -> Handle -> IO ()
writeRow v h = helper v h 0
    where helper v h n
            | n < V.length v = do
                writeTup (v V.! n) h
                helper v h $ n + 1
            | otherwise = return ()

-- coordinates start at (0,0) and top left corner
-- returns 0 when out of range
getIndex :: Int -> Int -> Int -> Int -> Int
getIndex h w x y
    | x >= 0 && x < w && y < h && y >= 0 = y * w + x
    | otherwise = 0



----- Library Functions -----
-----------------------------
createBlank :: Int -> Int -> PPM
createBlank h w = PPM pix h w 255
    where pix = V.replicate (w * h) (0, 0, 0)

writePixel :: PPM -> (Int, Int, Int) -> Int -> Int -> PPM
writePixel ppm w x y = PPM nPixels (height ppm) (width ppm) (cap ppm)
    where
        n = getIndex (height ppm) (width ppm) x y
        nPixels = (pixels ppm) V.// [(n, w)]

readPixel :: PPM -> Int -> Int -> (Int, Int, Int)
readPixel ppm x y = (pixels ppm) V.! n
    where n = getIndex (height ppm) (width ppm) x y


writeImage :: PPM -> Handle -> IO ()
writeImage img h = do
    hPutStrLn h "P3"
    hPutStrLn h $ (show (height img)) ++ " " ++ (show (width img))
    hPutStrLn h $ show $ cap img
    writeRow (pixels img) h 
