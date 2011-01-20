module Main (main) where

import Data.Word

clicks :: Double -> [Double]
clicks freq = [0, 1/freq..]

sample :: Double -> Double -> Double
sample f t = sin (2 * pi * f * t)

quantize :: Double -> Word16
quantize v = truncate $ fromIntegral (maxBound::Word16) * ((v + 1) / 2)

main = mapM_ print (take 44100 $ map (quantize . sample 440) (clicks 44100))
