module Main (main) where

import qualified Data.ByteString as B

main :: IO ()
main = B.putStr $ B.pack [0, 1, 2, 3]
