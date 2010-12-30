module Main (main) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Data.Word
import Data.Bits

data RIFFchunkDescriptor = RIFF String Word32 String

riffToByteString :: RIFFchunkDescriptor -> B.ByteString
riffToByteString (RIFF chunkID chunkSize format) =
    C.pack chunkID `B.append`
    (B.pack $ w32toW8 chunkSize) `B.append`
    C.pack format

w32toW8 :: Word32 -> [Word8]
w32toW8 i = [fromIntegral $ (i .&. 0x000000ff),
             fromIntegral $ (i .&. 0x0000ff00) `shiftR` 8,
             fromIntegral $ (i .&. 0x00ff0000) `shiftR` 16,
             fromIntegral $ (i .&. 0xff000000) `shiftR` 24]

main :: IO ()
main = B.putStr $ riffToByteString $ RIFF "RIFF" 4 "WAVE"
