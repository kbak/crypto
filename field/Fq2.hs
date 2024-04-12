module Fq2 where

import Field
import Fq

-- GF(p^2) = GF(p)[u] / (u^2 + 1), i.e., complex numbers
data Fq2 = Fq2 { fq2_0 :: Fq, fq2_1 :: Fq } deriving (Show, Eq, Ord)

instance Num Fq2 where
  Fq2 a0 a1 + Fq2 b0 b1 = Fq2 (a0 + b0) (a1 + b1)
  Fq2 a0 a1 - Fq2 b0 b1 = Fq2 (a0 - b0) (a1 - b1)
  Fq2 a0 a1 * Fq2 b0 b1 = Fq2 (a0 * b0 - a1 * b1) (a1 * b0 + a0 * b1)
  negate (Fq2 a0 a1)    = Fq2 (-a0) (-a1)
  abs (Fq2 a0 a1)       = Fq2 (abs a0) (abs a1)
  signum (Fq2 a0 a1)    = if a0 + a1 > 0 then 1 else 0
  fromInteger a         = Fq2 (fromInteger a) 0

instance Enum Fq2 where
  succ a   = a + Fq2 1 0
  pred a   = a - Fq2 1 0
  toEnum a = Fq2 (toEnum a) 0
  fromEnum (Fq2 a _) = fromEnum a

instance Real Fq2 where
  toRational a = fromIntegral $ fromEnum a

instance Integral Fq2 where
  quot a = fst.quotRem a
  rem  a = snd.quotRem a
  div  a = fst.divMod a
  mod  a = snd.divMod a
  quotRem a0 a1 = (q, a0 - a1 * q)
    where
    q = a0 * Field.inv a1
  divMod = quotRem
  toInteger (Fq2 a _) = toInteger a

instance Fractional Fq2 where
  a / b = a `div` b
  fromRational a = Fq2 (fromRational a) 0

instance Field Fq2 where
  (<^>) = (^)
  inv (Fq2 a0 a1) = Fq2 (a0 * factor) (-a1 * factor)
    where
    factor = inv (a1 * a1 + a0 * a0)
  toOne _  = Fq2 1 0
  toList (Fq2 a0 a1) = concatMap toList [a0, a1]
