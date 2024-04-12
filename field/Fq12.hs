module Fq12 where

import Field
import Fq6

-- GF(p^12) = GF(p^6)[w] / (w^2 - v)
-- Fq12 is constructed with Fq6(w) / (w^2 - γ) where γ = v
data Fq12 = Fq12 { fq12_0 :: Fq6, fq12_1 :: Fq6 } deriving (Show, Eq, Ord)

instance Num Fq12 where
  Fq12 a0 a1 + Fq12 b0 b1 = Fq12 (a0 + b0) (a1 + b1)
  Fq12 a0 a1 - Fq12 b0 b1 = Fq12 (a0 - b0) (a1 - b1)
  Fq12 a0 a1 * Fq12 b0 b1 = Fq12 (a0 * b0 + mulNonResFq6 (a1 * b1)) (a1 * b0 + a0 * b1)
  negate (Fq12 a0 a1)     = Fq12 (-a0) (-a1)
  abs (Fq12 a0 a1)        = Fq12 (abs a0) (abs a1)
  signum (Fq12 a0 a1)     = if a0 + a1 > 0 then 1 else 0
  fromInteger a           = Fq12 (fromInteger a) 0

instance Enum Fq12 where
  succ a   = a + Fq12 1 0
  pred a   = a - Fq12 1 0
  toEnum a = Fq12 (toEnum a) 0
  fromEnum (Fq12 a _) = fromEnum a

instance Real Fq12 where
  toRational a = fromIntegral $ fromEnum a

instance Integral Fq12 where
  quot a = fst.quotRem a
  rem  a = snd.quotRem a
  div  a = fst.divMod a
  mod  a = snd.divMod a
  quotRem a0 a1 = (q, a0 - a1 * q)
    where
    q = a0 * Field.inv a1
  divMod = quotRem
  toInteger (Fq12 a _) = toInteger a

instance Fractional Fq12 where
  a / b = a `div` b
  fromRational a = Fq12 (fromRational a) 0

instance Field Fq12 where
  (<^>) = (^)
  inv (Fq12 a0 a1) = Fq12 (a0 * factor) (-a1 * factor)
    where
    factor = inv (a0 * a0 - mulNonResFq6 (a1 * a1))
  toOne _  = Fq12 1 0
  toList (Fq12 a0 a1) = concatMap toList [a0, a1]
