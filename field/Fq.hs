module Fq where

import ModularArithmetic
import Field
import FqConfig (fieldModulus)

-- Finite field functions depend on a concrete value of `fieldModulus` that is
-- imported from `FqConfig`.

newtype Fq = Fq { fq :: Integer } deriving (Show, Ord)

instance Eq Fq where
  Fq a == Fq b = a `mod` fieldModulus == b `mod` fieldModulus
  n1  /= n2    = not (n1 == n2)

instance Num Fq where
  Fq a + Fq b   = fromInteger $ a + b
  Fq a - Fq b   = fromInteger $ a - b
  Fq a * Fq b   = fromInteger $ a * b
  negate (Fq a) = fromInteger (-a)
  abs    (Fq a) = fromInteger a
  signum (Fq a) = if a `mod` fieldModulus == 0 then 0 else 1
  fromInteger a = Fq (a `mod` fieldModulus)

instance Enum Fq where
  succ a = a + Fq 1
  pred a = a - Fq 1
  toEnum = fromInteger.fromIntegral
  fromEnum (Fq a) = fromIntegral a

instance Real Fq where
  toRational (Fq a) = fromIntegral a

instance Integral Fq where
  quot a = fst.quotRem a
  rem  a = snd.quotRem a
  div  a = fst.divMod a
  mod  a = snd.divMod a
  quotRem a b = (q, a - b * q)
    where
    q = a * Field.inv b
  divMod = quotRem
  toInteger (Fq a) = a

instance Fractional Fq where
  a / b = a `div` b
  fromRational a = Fq (truncate a `mod` fieldModulus)

instance Field Fq where
  (Fq a) <^> b  = Fq $ pow a b fieldModulus
  inv (Fq a)    = Fq $ ModularArithmetic.inv a fieldModulus
  toOne _       = Fq 1
  toList (Fq a) = [a]

multiInv as = map Fq $ ModularArithmetic.multiInv (map toInteger as) fieldModulus

sqrt (Fq a) = ModularArithmetic.sqrt a fieldModulus >>= (\(s1, s2) -> Just (Fq s1, Fq s2))