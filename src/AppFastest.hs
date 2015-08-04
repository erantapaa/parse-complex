{-# LANGUAGE BangPatterns #-}

module AppFastest
where

import PPM
import System.IO
import Control.Applicative as A
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.ByteString.Lazy as LP

import Data.ByteString.Lex.Double (readDouble)
import Control.Monad

import qualified Data.Vector.Unboxed as UnboxedV
import qualified Data.Vector.Unboxed.Mutable as UnboxedM

parseComplex' = do
  r <- P.takeTill (== '+')
  P.char '+'
  i <- P.takeTill (== '*')
  -- P.string iT  -- or just skip this
  return $ convCoord (computeIndex (toDouble r)) (computeIndex (toDouble i))
  where toDouble s = case readDouble s of
                       Nothing -> 0
                       Just (d, _) -> d

---------- IMAGE PROCESSING ----------

bounds = 2.0 :: Double
imgSize = 400 :: Int

intensity = 300.0 :: Double
falloff = 0.8 :: Double
(r, g, b) = (255.0, 76.5, 25.5) :: (Double, Double, Double)

computeIndex :: Double -> Int
computeIndex x = (+) 1 $ floor $ (x + bounds) / (2 * bounds / fromIntegral imgSize)

convCoord ::Int -> Int -> Int
convCoord h v = v * imgSize + h

genVec :: [Int] -> UnboxedV.Vector Int
genVec xs = runST $ do
  mv <- UnboxedM.replicate (imgSize*imgSize) (0::Int)
  forM_ xs $ \x -> do 
    count <- UnboxedM.unsafeRead mv x
    UnboxedM.unsafeWrite mv x (count+1)
  UnboxedV.freeze mv

extr :: LP.Result a -> a
extr (LP.Done _ r) = r
extr (LP.Fail _ _ y) = error y

computeVector = genVec . map (extr.LP.parse parseComplex') . LB.words 

---------- MAIN ----------

mainLength path = do
    rawData <- liftA LB.words (LB.readFile path)
    let formatedData = map (extr.LP.parse parseComplex') rawData
    print $ length formatedData

mainSum path = do
    rawData <- liftA LB.words (LB.readFile path)
    let formatedData = map (extr.LP.parse parseComplex') rawData
    print $ sum $ formatedData

mainVectorSum path = do
    rawData <- liftA LB.words (LB.readFile path)
    let formatedData = map (extr.LP.parse parseComplex') rawData
        v = genVec formatedData 
    print $ UnboxedV.sum v 

mainVectorLength path = do
    rawData <- liftA LB.words (LB.readFile path)
    let formatedData = map (extr.LP.parse parseComplex') rawData
        v = genVec formatedData 
    print $ UnboxedV.length v 

