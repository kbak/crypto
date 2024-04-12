{-# LANGUAGE TemplateHaskell #-}
-- ^^^ for QuickCheck.quickCheckAll

module Fq6Test where

import FqConfig
import Field
import Fq
import Fq2
import Fq6
import Test.QuickCheck

one = Fq6 1 0 0
x   = one
f   = Fq6 (Fq2 1 2) (Fq2 3 4) (Fq2 5 6)
fpx = Fq6 (Fq2 2 2) (Fq2 3 4) (Fq2 5 6)

-- the inverse of 1 should be 1
prop_invOne = inv one == one

degree = 6

prop_add      = x + f == fpx
prop_ assoc   = one * f + x * f == (one + x) * f
prop_powx     = x <^> (fieldModulus^degree - 1) == one
prop_powf     = f <^> (fieldModulus^degree - 1) == one
prop_div      = f / f == one
prop_divassoc = one / f + x / f == (one + x) / f

-- f*f*...*f == f^n
prop_powIterateFq a0 a1 a2 a3 a4 a5 n = n /= 0 ==>
  iterate (e*) e !! (n' - 1) == e <^> n'
  where
  e  = Fq6 (Fq2 (Fq a0) (Fq a1))
           (Fq2 (Fq a2) (Fq a3))
           (Fq2 (Fq a4) (Fq a5))
  n' = abs n

-- f * f^(n-1) == f^n
prop_powMultFq a0 a1 a2 a3 a4 a5 n = n /= 0 ==>
  e * (e <^> (n' - 1)) == e <^> n'
  where
  e  = Fq6 (Fq2 (Fq a0) (Fq a1))
           (Fq2 (Fq a2) (Fq a3))
           (Fq2 (Fq a4) (Fq a5))
  n' = abs n

return []
runTests = $quickCheckAll