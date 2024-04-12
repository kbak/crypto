module Fqp where

-- extension field of polynomials over the finite field
-- https://medium.com/@VitalikButerin/exploring-elliptic-curve-pairings-c73c1864e627
-- https://hackmd.io/@benjaminion/bls12-381 (really good!)

import FqConfig
import Fq
import Field
import Polynomial -- if ghci fails, run `:set -imath`
import Data.Bits

data Fqp = Fqp { fqp :: [Fq], pmod :: [Fq] } deriving (Show, Ord)

instance Eq Fqp where
  (Fqp p1 _) == (Fqp p2 _) = coeffclean p1 == coeffclean p2
  p1 /= p2 = not (p1 == p2)

instance Num Fqp where
  Fqp a m + Fqp b [] = Fqp (a `polyadd` b) m
  Fqp a _ + Fqp b m  = Fqp (a `polyadd` b) m
  Fqp a m - Fqp b [] = Fqp (a `polysub` b) m
  Fqp a _ - Fqp b m  = Fqp (a `polysub` b) m
  Fqp a m * Fqp b [] = mulFqp a b m
  Fqp a _ * Fqp b m  = mulFqp a b m
  negate (Fqp a m)   = Fqp (polyscale (-1) a) m
  abs                = polyclean
  signum (Fqp a _)   = if isZero a then 0 else 1
  fromInteger a      = newFqp [a] []

-- multiplication modulo: a * b `mod` m
mulFqp a b m = polyclean $ modFqp $ Fqp (a `polymult` b) m

instance Enum Fqp where
  succ (Fqp a m) = Fqp ((head a + 1) : tail a) m
  pred (Fqp a m) = Fqp ((head a - 1) : tail a) m
  toEnum a       = newFqp [toEnum a] []
  fromEnum (Fqp [] _) = 0
  fromEnum (Fqp (x:_) _) = fromEnum x

instance Real Fqp where
  toRational a = fromIntegral $ fromEnum a

instance Integral Fqp where
  quot a = fst.quotRem a
  rem  a = snd.quotRem a
  div  a = fst.divMod a
  mod  a = snd.divMod a
  quotRem a b = (q, a - b * q)
    where
    q = a * inv b
  divMod = quotRem
  toInteger (Fqp [] _) = 0
  toInteger (Fqp (x:_) _) = toInteger x

instance Fractional Fqp where
  a / b = a `div` b
  fromRational a = newFqp [truncate a] []

instance Field Fqp where
  a <^> b  = a `powFqp` b
  inv      = invFqp
  toOne a  = Fqp [1] (pmod a)
  toList a = concatMap toList (fqp a)

-- builds a polynomial from coefficients
newFqp cs ms = Fqp (map fromInteger cs) (map fromInteger ms)

-- cleans up coefficients and calculates degree
deg coeffs = if l > 0 then l - 1 else 0
  where
  l = length $ coeffclean coeffs

-- cleans up leading zero's from the polynomial
polyclean p = p{ fqp = coeffclean $ fqp p }

-- computes p `mod` m for polynomials
-- IMPORTANT: it assumes that in `modulus`, c * x^n for the highest n, c = 1
modFqp poly@(Fqp p m)
  | length p' < degM = poly
  | otherwise        = modFqp poly{ fqp = init $ p `polysub` nMod }
  where
  degM = 1 + deg m
  p'   = coeffclean p
  -- scale the modulus
  sMod = last p `polyscale` m
  -- "shift" to align with the highest coefficient
  nMod = replicate (length p - degM) 0 ++ sMod

-- computes a^x `mod` m where a is a polynomial and x is a scalar
powFqp a x = pow' a x a{ fqp = [1] }
  where
  pow' _ 0 res  = modFqp res
  pow' b e res
    | odd e     = pow' b' e' (res * b)
    | otherwise = pow' b' e' res
    where
    b' = b * b
    e' = e `shiftR` 1

-- computes the inverse of p `mod` m for polynomials
-- polynomial version of extended Euclides algorithm for finite fields
invFqp poly@(Fqp p m) = invFqp' one zero poly{ fqp = p ++ [0] } poly{ fqp = m }
  where
  d    = deg m
  deg1 = d + 1
  one  = Fqp (1 : replicate d 0) m
  zero = Fqp (replicate deg1 0) m
  invFqp' pLm@(Fqp lm _) (Fqp hm _) pLow@(Fqp low _) (Fqp high _)
    | deg low == 0 = Fqp (coeffclean $ inv (head low) `polyscale` take d lm) m
    | otherwise    = invFqp' nm pLm low' pLow
    where
    r     = fst $ coeffclean high `polydiv` coeffclean low
    r'    = r ++ replicate (deg1 - length r) 0
    nm    = Fqp (hm `polysub` (lm `polymult` r')) m
    low'  = Fqp (high `polysub` (low `polymult` r')) m

fqp2  cs = newFqp cs fq2Modulus
fqp12 cs = newFqp cs fq12Modulus