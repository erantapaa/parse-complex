
module Random where

import Control.Monad
import System.Random
import Text.Printf

bounds = 1.9 :: Float

-- an infinte stream of random floats
randomFloats g =
  let (a,g') = randomR (-bounds, bounds) g in a : randomFloats g'

-- an infinite stream of random pairs
randomPairs g =
  let (a,g') = randomR (-bounds, bounds) g
      (b,g'') = randomR (-bounds, bounds) g'
  in (a,b) : randomPairs g''

emitRandomPairs' n g = do
  forM_ (take n $ randomPairs g) $ \(a,b) -> do
    putStrLn $ printf "%.6f" a  ++ "+" ++ printf "%.6f" b ++ "*I"

emitRandomPairs n seed = emitRandomPairs' n (mkStdGen seed)

