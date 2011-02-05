module Main (main) where

envelope :: Double -> Double -> Double -> Double -> Double -> Double -> [Double]
envelope rate dur a d s r =
    attack ++ decay ++ sustain ++ release
    where
      attack = init [0, 1/(rate*a)..1]
      decay = init [1, 1 - 1/(rate*d)..s]
      sustain = replicate (truncate ((dur - a - d) * rate)) s
      release = init [s, s - 1/(rate*d)..0]

main = do
  mapM_ print $ envelope 44100 0.5 0.05 0.01 0.9 0.07
  mapM_ print $ envelope 44100 1.5 0.5 0.1 0.7 0.1
