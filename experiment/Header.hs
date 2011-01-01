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

data RIFFHeader = RIFF Word32

instance Packable RIFFHeader where
    toByteString (RIFF chunkSize) =
        C.pack "RIFF" `B.append`
        toByteString chunkSize `B.append`
        C.pack "WAVE"

main :: IO ()
main = B.putStr $ toByteString $ RIFF 4
