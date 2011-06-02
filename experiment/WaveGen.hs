module Main (main) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Data.Word
import Data.Int
import Data.Bits

byteSelect :: (Bits a, Num b, Integral a) => Int -> a -> b
byteSelect idx b =
    fromIntegral $ (b .&. (0xff `shiftL` (idx * 8))) `shiftR` (idx * 8)

class Packable a where
    toByteString :: a -> B.ByteString

instance Packable Word32 where
    toByteString i =
        foldr (\idx b -> byteSelect idx i `B.cons` b) B.empty [0..3]

instance Packable Word16 where
    toByteString i =
        foldr (\idx b -> byteSelect idx i `B.cons` b) B.empty [0..1]

instance Packable Int16 where
    toByteString i =
        foldr (\idx b -> byteSelect idx i `B.cons` b) B.empty [0..1]

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
        toByteString chunkSize `B.append`
        toByteString audioFormat `B.append`
        toByteString numChannels `B.append`
        toByteString sampleRate `B.append`
        toByteString byteRate `B.append`
        toByteString blockAlign `B.append`
        toByteString bitsPerSample `B.append`
        toByteString extraParamSize

data DataChunkHeader = DATA Word32
instance Packable DataChunkHeader where
    toByteString (DATA chunkSize) =
        C.pack "data" `B.append`
        toByteString chunkSize

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

genWave :: (Double -> Double -> Double) -> Double -> Double -> Double ->
           [Double]
genWave func rate freq dur =
    take (truncate (rate * dur)) $ map (func freq) (clicks rate)

putWave :: [Double]-> IO()
putWave = mapM_ (B.putStr . toByteString . quantize)

main :: IO ()
main = do
  let formatChunk = FMT 18 1 1 44100 88200 2 16 0
      packedFmt = toByteString formatChunk
      num_samples = 44100
      riffChunk = RIFF (fromIntegral (4 +
                                      B.length packedFmt +
                                      4 + 4 + num_samples * 8 * 2))
      dataChunkHeader = DATA (fromIntegral num_samples * 8 * 2)
      rate = 44100
  B.putStr $ toByteString riffChunk
  B.putStr packedFmt
  B.putStr $ toByteString dataChunkHeader
  putWave $ genWave sawwave rate 261.6 1
  putWave $ genWave sinwave rate 293.7 1
  putWave $ genWave sawwave rate 329.6 1
  putWave $ genWave sinwave rate 349.2 1
  putWave $ genWave sawwave rate 392.0 1
  putWave $ genWave sinwave rate 440.0 1
  putWave $ genWave sawwave rate 493.9 1
  putWave $ genWave sinwave rate 523.3 1
