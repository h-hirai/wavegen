module Main (main) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Data.Word
import Data.Bits

w32toW8 :: Word32 -> [Word8]
w32toW8 i = [fromIntegral $ (i .&. 0x000000ff),
             fromIntegral $ (i .&. 0x0000ff00) `shiftR` 8,
             fromIntegral $ (i .&. 0x00ff0000) `shiftR` 16,
             fromIntegral $ (i .&. 0xff000000) `shiftR` 24]

w32toByteString :: Word32 -> B.ByteString
w32toByteString i =
    fromIntegral (i .&. 0x000000ff) `B.cons`
    (fromIntegral ((i .&. 0x0000ff00) `shiftR` 8) `B.cons`
     (fromIntegral ((i .&. 0x00ff0000) `shiftR` 16) `B.cons`
      (fromIntegral ((i .&. 0xff000000) `shiftR` 24) `B.cons`
       B.empty)))

main :: IO ()
main = do
  B.putStr $ B.pack $ w32toW8 0x00010203
  B.putStr $ C.pack "String"
  B.putStr $ w32toByteString 0x04050607
