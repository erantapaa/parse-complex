module Main where

import System.Environment
import qualified Random as R
import qualified AppOriginal
import qualified AppFaster
import qualified AppFastest
import qualified App
import qualified Parallel

r1m = "input/r1m"

main :: IO ()
main = do
  (arg1:args) <- getArgs
  case arg1 of
    "rand"          -> generateRandoms args
    "orig-ppm"      -> AppOriginal.main r1m
    "orig-length"   -> AppOriginal.mainLength r1m
    "orig-sum"      -> AppOriginal.mainSum r1m
    "orig-vector"   -> AppOriginal.mainVector r1m
    "faster-ppm"    -> AppFaster.main r1m
    "faster-length" -> AppFaster.mainLength r1m
    "faster-sum"    -> AppFaster.mainSum r1m
    "faster-vlength" -> AppFaster.mainVectorLength r1m
    "faster-vsum"   -> AppFaster.mainVectorSum r1m

    "fastest-length" -> AppFastest.mainLength r1m
    "fastest-sum"    -> AppFastest.mainSum r1m
    "fastest-vlength" -> AppFastest.mainVectorLength r1m
    "fastest-vsum"   -> AppFastest.mainVectorSum r1m
    "ppm"            -> App.main [r1m]
    "parallel-test"  -> Parallel.test2

generateRandoms :: [String] -> IO ()
generateRandoms args = do
  let (n:seed:_) = map read (args ++ ["1000", "1000"]) 
  R.emitRandomPairs n seed

