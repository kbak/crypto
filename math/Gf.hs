module Gf where
-- generic version of finite field (Fq), i.e., it doesn't depend on importing a global FqConfig

import ModularArithmetic
import Field

data Gf = Z { gf :: Integer, gfmod :: Integer } deriving (Show, Ord)

intGf n m = Z (n `mod` m) m

instance Eq Gf where
  Z a m == Z b _ = a `mod` m == b `mod` m
  n1  /= n2      = not (n1 == n2)

instance Num Gf where
  Z a m + Z b _  = intGf (a + b) m
  Z a m - Z b _  = intGf (a - b) m
  Z a m * Z b _  = intGf (a * b) m
  negate (Z a m) = intGf (-a) m
  abs    (Z a m) = intGf a m
  signum (Z a m) = if a `mod` m == 0 then 0 else 1
  fromInteger a  = Z (abs a) undefined

instance Enum Gf where
  succ a   = a + 1
  pred a   = a - 1
  toEnum a = Z (fromIntegral $ abs a) undefined
  fromEnum (Z a _) = fromIntegral a

instance Real Gf where
  toRational (Z a _) = fromIntegral a

instance Integral Gf where
  quot a = fst.quotRem a
  rem  a = snd.quotRem a
  div  a = fst.divMod a
  mod  a = snd.divMod a
  quotRem a b = (q, a - b * q)
    where
    q = a * Field.inv b
  divMod = quotRem
  toInteger (Z a _) = a

instance Fractional Gf where
  a / b = a `div` b
  fromRational a = Z ((abs.truncate) a) undefined

instance Field Gf where
  (Z a m) <^> b = Z (pow a b m) m
  inv (Z a m)   = Z (ModularArithmetic.inv a m) m
  toOne (Z _ m) = Z 1 m
  toList (Z a _) = [a]

multiInv as = map (`Z` m) $ ModularArithmetic.multiInv (map gf as) m
  where
  m = gfmod $ head as

sqrt (Z a m) = ModularArithmetic.sqrt a m >>= (\(s1, s2) -> Just (Z s1 m, Z s2 m))