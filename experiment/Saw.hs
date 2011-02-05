module Main (main) where

clicks :: Double -> [Double]
clicks freq = [0, 1/freq..]

sawwave :: Double -> Double -> Double
sawwave f t =
    if t > 1 / f then sawwave f (t - 1 / f)
    else 2 * f * t - 1

main = do
  let rate = 44100
      freq = 440
      wave = map (sawwave freq) (clicks rate)
  mapM_ print $ take 1000 wave
