module Fq6 where

import Field
import FqConfig (mulNonResFq2)
import Fq2

-- GF(p^6) = GF(p^2)[v] / (v^3 - u - 1), i.e, Fq6 is constructed with
-- Fq2(v) / (v^3 - ξ) where ξ = u + 1
data Fq6 = Fq6 { fq6_0 :: Fq2, fq6_1 :: Fq2, fq6_2 :: Fq2 } deriving (Show, Eq, Ord)

instance Num Fq6 where
  Fq6 a0 a1 a2 + Fq6 b0 b1 b2 = Fq6 (a0 + b0) (a1 + b1) (a2 + b2)
  Fq6 a0 a1 a2 - Fq6 b0 b1 b2 = Fq6 (a0 - b0) (a1 - b1) (a2 - b2)
  Fq6 a0 a1 a2 * Fq6 b0 b1 b2 = Fq6 (t0 + t3) (t1 + t4) t2
    where
    t0 = a0 * b0
    t1 = a0 * b1 + a1 * b0
    t2 = a0 * b2 + a1 * b1 + a2 * b0
    t3 = mulNonResFq2' (a1 * b2 + a2 * b1)
    t4 = mulNonResFq2' (a2 * b2)
  negate (Fq6 a0 a1 a2) = Fq6 (-a0) (-a1) (-a2)
  abs (Fq6 a0 a1 a2)    = Fq6 (abs a0) (abs a1) (abs a2)
  signum (Fq6 a0 a1 a2) = if a0 + a1 + a2 > 0 then 1 else 0
  fromInteger a         = Fq6 (fromInteger a) 0 0

instance Enum Fq6 where
  succ a   = a + Fq6 1 0 0
  pred a   = a - Fq6 1 0 0
  toEnum a = Fq6 (toEnum a) 0 0
  fromEnum (Fq6 a _ _) = fromEnum a

instance Real Fq6 where
  toRational a = fromIntegral $ fromEnum a

instance Integral Fq6 where
  quot a = fst.quotRem a
  rem  a = snd.quotRem a
  div  a = fst.divMod a
  mod  a = snd.divMod a
  quotRem a0 a1 = (q, a0 - a1 * q)
    where
    q = a0 * Field.inv a1
  divMod = quotRem
  toInteger (Fq6 a _ _) = toInteger a

instance Fractional Fq6 where
  a / b = a `div` b
  fromRational a = Fq6 (fromRational a) 0 0

instance Field Fq6 where
  (<^>) = (^)
  inv (Fq6 a0 a1 a2) = Fq6 (t0 * factor) (t1 * factor) (t2 * factor)
    where
    t0 = a0 * a0 - mulNonResFq2' (a1 * a2)
    t1 = mulNonResFq2' (a2 * a2) - a0 * a1
    t2 = a1 * a1 - a0 * a2
    factor = inv (a0 * t0 + mulNonResFq2' (a2 * t1 + a1 * t2))
  toOne _  = Fq6 1 0 0
  toList (Fq6 a0 a1 a2) = concatMap toList [a0, a1, a2]

mulNonResFq2' (Fq2 a0 a1) = Fq2 a0' a1'
  where
  (a0', a1') = mulNonResFq2 a0 a1

-- Given a + bv + cv^2, we multiplity it by v (because of the F12) and
-- this produces av + bv^2 + cv^3
-- but because v^3 = u + 1, we have c(1 + u) + av + bv^2
mulNonResFq6 (Fq6 a0 a1 a2) = Fq6 (mulNonResFq2' a2) a0 a1