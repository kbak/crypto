module Field where

import Data.Bits

-- class for elements that can do (quick) modulo operations
class Num a => Field a where
   -- modular exponentiation
   (<^>) :: (Integral b, Data.Bits.Bits b) => a -> b -> a
   -- modular inversion: a => 1/a
   inv :: a -> a
   -- based on an element, returns the element one in Fp
   toOne :: a -> a
   -- returns a list of integers that make up the field element
   toList :: a -> [Integer]