module Main (main) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Data.Word
import Data.Bits

class Packable a where
    toByteString :: a -> B.ByteString

instance Packable Word32 where
    toByteString i =
        fromIntegral (i .&. 0x000000ff) `B.cons`
        (fromIntegral ((i .&. 0x0000ff00) `shiftR` 8) `B.cons`
         (fromIntegral ((i .&. 0x00ff0000) `shiftR` 16) `B.cons`
          (fromIntegral ((i .&. 0xff000000) `shiftR` 24) `B.cons`
           B.empty)))

instance Packable Word16 where
    toByteString i =
        fromIntegral (i .&. 0x00ff) `B.cons`
        (fromIntegral ((i .&. 0xff00) `shiftR` 8) `B.cons`
         B.empty)

data RIFFchunk = RIFF Word32

instance Packable RIFFchunk where
    toByteString (RIFF chunkSize) =
        C.pack "RIFF" `B.append`
        toByteString chunkSize `B.append`
        C.pack "WAVE"

data FormatChunk = FMT Word32 Word16 Word16 Word32 Word32 Word16 Word16 Word16

instance Packable FormatChunk where
    toByteString (FMT chunkSize
                      audioFormat
                      numChannels
                      sampleRate
                      byteRate
                      blockAlign
                      bitsPerSample
                      extraParamSize) =
        C.pack "fmt " `B.append`
        toByteString audioFormat `B.append`
        toByteString numChannels `B.append`
        toByteString sampleRate `B.append`
        toByteString byteRate `B.append`
        toByteString blockAlign `B.append`
        toByteString bitsPerSample `B.append`
        toByteString extraParamSize

main :: IO ()
main = do
  let formatChunk = FMT 18 1 1 44100 88200 2 16 0
      packedFmt = toByteString formatChunk
  B.putStr $ toByteString $ RIFF $ fromIntegral $ B.length packedFmt + 4
  B.putStr packedFmt
