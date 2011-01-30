module Main (main) where

import Data.Int

clicks :: Double -> [Double]
clicks freq = [0, 1/freq..]

sample :: Double -> Double -> Double
sample f t = sin (2 * pi * f * t)

quantize :: Double -> Int16
quantize v = truncate $ fromIntegral (maxBound::Int16) * v

main = mapM_ print (take 500 $ map (quantize . sample 440) (clicks 44100))
