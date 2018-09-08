module Random where

import System.Random

randomInt :: Int -> Int -> StdGen -> (Int, StdGen)
randomInt low high gen = randomR (low, high) gen
