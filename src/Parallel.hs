
module Parallel
where

import Control.Parallel.Strategies
import qualified Data.ByteString.Lazy.Char8 as LB
import System.IO    (Handle,stdout,openBinaryFile,hClose,hFileSize,IOMode(..), hClose, hTell, SeekMode(..), hSeek)
import Data.Int

import AppFastest (computeVector)
import qualified Data.Vector.Unboxed as UnboxedV
import Control.DeepSeq

wordCount = length . LB.words

-- print where the middle of the file starts
test1 path = do
  h <- openBinaryFile path ReadMode
  size <- hFileSize h
  let half = size `div` 2
  hSeek h AbsoluteSeek half
  line <- LB.hGet h 1024
  let Just ind = LB.findIndex (=='\n') line
      realHalf = half + 1 + (fromIntegral ind)
  print realHalf
  hClose h

test2 = do
  let mid = 10500191
      path = "input/r1m"
  content1 <- fmap (LB.take mid) $ LB.readFile path
  h2 <- openBinaryFile path ReadMode
  hSeek h2 AbsoluteSeek (fromIntegral mid)
  content2 <- LB.hGetContents h2

  let v = runEval $ do
            v1 <- rpar $ force $ computeVector content1
            v2 <- rpar $ force $ computeVector content2
            rseq v1
            rseq v2
            return $ UnboxedV.zipWith (+) v1 v2
  print $ UnboxedV.sum v 

