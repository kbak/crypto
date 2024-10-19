module GCf where
-- generic version of finite field (Fq), i.e., it doesn't depend on importing a global FqConfig
-- modulus values are centered around zero. For example, for p=4 normally values are [0, 1, 2, 3]
-- whereas centered around zero are [-2, -1, 0, 1]

import ModularArithmetic
import Field

data GCf = GCf { gcf :: Integer, gcfmod :: Integer } deriving (Show, Ord)

-- centered modular arithmetic function 
-- i.e., returns a number modulo m centered around 0
cMod x m
  | result >  halfM = result - m
  | result < -halfM = result + m
  | otherwise       = result
  where
  halfM = m `div` 2
  result = (x `mod` m + halfM) `mod` m - halfM   

intGCf n m = GCf (n `cMod` m) m

instance Eq GCf where
  GCf a m == GCf b _ = a `cMod` m == b `cMod` m
  n1  /= n2      = not (n1 == n2)

instance Num GCf where
  GCf a m + GCf b _  = intGCf (a + b) m
  GCf a m - GCf b _  = intGCf (a - b) m
  GCf a m * GCf b _  = intGCf (a * b) m
  negate (GCf a m) = intGCf (-a) m
  abs    (GCf a m) = intGCf a m
  signum (GCf a m) = if a `cMod` m == 0 then 0 else 1
  fromInteger a  = GCf (abs a) undefined

instance Enum GCf where
  succ a   = a + 1
  pred a   = a - 1
  toEnum a = GCf (fromIntegral $ abs a) undefined
  fromEnum (GCf a _) = fromIntegral a

instance Real GCf where
  toRational (GCf a _) = fromIntegral a

instance Integral GCf where
  quot a = fst.quotRem a
  rem  a = snd.quotRem a
  div  a = fst.divMod a
  mod  a = snd.divMod a
  quotRem a b = (q, a - b * q)
    where
    q = a * Field.inv b
  divMod = quotRem
  toInteger (GCf a _) = a

instance Fractional GCf where
  a / b = a `div` b
  fromRational a = GCf ((abs.truncate) a) undefined

instance Field GCf where
  (GCf a m) <^> b = GCf (pow a b m) m
  inv (GCf a m)   = GCf (ModularArithmetic.inv a m) m
  toOne (GCf _ m) = GCf 1 m
  toList (GCf a _) = [a]

multiInv as = map (`GCf` m) $ ModularArithmetic.multiInv (map gcf as) m
  where
  m = gcfmod $ head as

sqrt (GCf a m) = ModularArithmetic.sqrt a m >>= (\(s1, s2) -> Just (GCf s1 m, GCf s2 m))