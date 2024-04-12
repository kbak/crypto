module Basic where

import Data.List
import Data.Array
import Data.Bits((.&.))

-- dot product of two vectors
dotProduct a s = sum $ zipWith (*) a s

-- rounds a fractional number up to `digits` digits
roundFrac digits f = fromInteger (round $ f * (10^digits)) / (10.0^^digits)

swap (a, b) = (b, a)

-- splits a list into chunks of given length
chunks n = takeWhile (not . null) . Data.List.unfoldr (Just . splitAt n)

listToArray l = listArray (0, fromIntegral (length l) - 1) l

cycleL n xs = r ++ l
    where
    (l, r) = splitAt n xs

isPower2 n = n .&. (n-1) == 0