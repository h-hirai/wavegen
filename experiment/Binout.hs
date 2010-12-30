module Main (main) where

import qualified Data.ByteString as B

import Data.Word
import Data.Bits

w32toW8 :: Word32 -> [Word8]
w32toW8 i = [fromIntegral $ (i .&. 0xff000000) `shiftR` 24,
             fromIntegral $ (i .&. 0x00ff0000) `shiftR` 16,
             fromIntegral $ (i .&. 0x0000ff00) `shiftR` 8,
             fromIntegral $ (i .&. 0x000000ff)]

main :: IO ()
main = B.putStr $ B.pack $ w32toW8 0x00010203
