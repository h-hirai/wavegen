module Main (main) where

import Data.Int
import Data.Bits
import qualified Data.ByteString as B

class Packable a where
    toByteString :: a -> B.ByteString

instance Packable Int16 where
    toByteString i =
        fromIntegral (i .&. 0x00ff) `B.cons`
        (fromIntegral ((i .&. 0xff00) `shiftR` 8) `B.cons`
         B.empty)

clicks :: Double -> [Double]
clicks freq = [0, 1/freq..]

sinwave :: Double -> Double -> Double
sinwave f t = sin (2 * pi * f * t)

sawwave :: Double -> Double -> Double
sawwave f t =
    if t > 1 / f then sawwave f (t - 1 / f)
    else 2 * f * t - 1

quantize :: Double -> Int16
quantize v = truncate $ fromIntegral (maxBound::Int16) * v

main = do
  let rate = 44100
      freq = 440
      wave = map (sinwave freq) (clicks rate)
      pack = toByteString . quantize
  mapM_ B.putStr $ take 100 $ map pack wave
