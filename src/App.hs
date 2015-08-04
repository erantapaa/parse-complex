{- LANGUAGE BangPatterns #-}

module App where

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
import Control.Monad
import Data.List
import Data.ByteString.Lex.Double (readDouble)


apply :: a -> (a -> b) -> b
apply a = \f -> f a


---------- PARSING ----------

iT = B.pack "*I"

data Complex = Complex !Double Double
    deriving (Show)

cAdd:: Complex -> [Complex] -> Complex
cAdd (Complex a b) [(Complex c d)] = Complex (a + c) (b + d)
cAdd c _ = c
cConv :: Complex -> Complex
cConv (Complex a b) = Complex b a

parseTerm :: P.Parser Complex
parseTerm = parseDouble <*> parseConv
    where 
        parseDouble = liftA (\x -> apply (Complex x 0)) $
            liftA2 (\x y -> read ((x:y) ++ "0")) P.anyChar $ many $ P.satisfy $ \x -> P.isDigit x || x == '.'
        parseConv = P.option id $ liftA (const cConv) $ P.string iT

test :: P.Parser String
test = liftA2 (\x y -> (x:y)) P.anyChar $ many $ P.satisfy $ \x -> P.isDigit x || x == '.'

parseComplex :: P.Parser Complex
parseComplex = liftA2 cAdd parseTerm $ many $ many (P.char '+') A.*> parseTerm

parseComplex' = do
  r <- P.takeTill (== '+')
  P.char '+'
  i <- P.takeTill (== '*')
  P.string iT  -- or just skip this
  return $ Complex (toDouble r) (toDouble i)
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

convCoord ::(Int, Int) -> Int
convCoord (h, v) = v * imgSize + h

computeCIndex :: Complex -> Int
computeCIndex (Complex r i) = convCoord (computeIndex r, computeIndex i)

genVec :: [Complex] -> V.Vector Int
genVec xs = runST $ do
    mv <- M.replicate (imgSize ^ 2) 0
    mapM_ (incr mv) $ map computeCIndex xs -- unsafeModify
    V.freeze mv
        where
            incr mv i = do
                pre <- M.read mv i -- unsafeRead?
                M.unsafeWrite mv i $ pre + 1

colorFunction :: Double -> (Int, Int, Int)
colorFunction d = (floor (y * r), floor(y * g), floor(y * b))
    where y = (intensity * d) ** falloff

colorFunction1 n = (n, n, n)

genImage :: V.Vector Int -> PPM
genImage v = PPM (V.map (\x -> colorFunction((fromIntegral x) / (fromIntegral mx))) v) imgSize imgSize 255
    where mx = V.maximum v

extr :: LP.Result a -> a
extr (LP.Done _ r) = r
extr (LP.Fail _ _ y) = error y

---------- MAIN ----------

main args = do
    let (path:_) = args
    rawData <- liftA LB.words (LB.readFile path)
    let formatedData = map (extr.LP.parse parseComplex') rawData
    let rpart (Complex r i) = r
        sum' = foldl' (+) 0
    print $ sum' (map rpart formatedData)
{-
    forM_ formatedData $ \c -> do
      let i = computeCIndex c
      when (i > 160000) $ do
        putStrLn $ "bad complex: " ++ show c ++ " -> " ++ show i
-}
{-
    h <- openFile "test.ppm" WriteMode
    writeImage (genImage (genVec formatedData)) h
    hClose h
-}
