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

l :: [Int16]
l = [0, 1, -1,
     maxBound, minBound,
     maxBound + 1, minBound - 1,
     maxBound - 1, minBound + 1]

main :: IO ()
main = mapM_ B.putStr $ map toByteString l
