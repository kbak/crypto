module Blake where

import Numeric (showHex)
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Crypto.Hash.BLAKE2.BLAKE2s
import GHC.Word

-- the length of Blake2s hash is 32 bytes
hashLen = 32

-- computes Blake2s hash for a given ByteString
blake = hash hashLen mempty

-- takes a ByteString or Integer and converts to ByteString
-- important, Integers must fit in `hashLen` bytes
toBytes (Left bs) = bs
toBytes (Right n) = intToBytes hashLen n

-- converts n :: Integer to ByteString of length `numBytes`
intToBytes :: Int -> Integer -> ByteString
intToBytes numBytes n = B.pack $ map ((fromIntegral . shiftR n) . (8 *)) $ reverse [0 .. numBytes - 1]

intToBytes32 n = B.pack $ map (fromIntegral . shiftR n) [248,240,232,224,216,208,200,192,184,176,168,160,152,144,136,128,120,112,104,96,88,80,72,64,56,48,40,32,24,16,8,0]

-- converts a ByteString to Integer
bytesToInt :: ByteString -> Integer
bytesToInt = B.foldl' (\acc b -> (acc `shiftL` 8) .|. fromIntegral b) 0

-- converts ByteString to String in hex format
toHex = B.foldr showHex ""

getPseudorandomIndices seed m count excludeMultiplesOf
    | m >= 2^24               = error "Modulus too big"
    | excludeMultiplesOf == 0 = map (`mod` m) cs
    | otherwise               = [x + 1 + (x `div` (excludeMultiplesOf - 1)) | x <- map (`mod` rm) cs]
    where
    input = until (\inp -> B.length inp >= 4 * count)
                  (\inp -> B.append inp $ blake $ takeEnd hashLen inp) seed
    cs = map bytesToInt $ take count $ chunksB 4 input
    rm = m * (excludeMultiplesOf - 1) `div` excludeMultiplesOf

-- splits a ByteString into chunks of given length
chunksB n xs
    | B.null xs = []
    | otherwise = ys : chunksB n zs
    where
    (ys, zs) = B.splitAt n xs

takeEnd n bs = B.drop (B.length bs - n) bs