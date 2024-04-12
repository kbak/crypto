{-# LANGUAGE TemplateHaskell #-}
-- ^^^ for QuickCheck.quickCheckAll

module Fq2Test where

import FqConfig
import Field
import Fq2
import Fq
import Test.QuickCheck

one = Fq2 1 0
x   = one
f   = Fq2 1 2
fpx = Fq2 2 2

-- the inverse of 1 should be 1
prop_invOne = inv one == one

degree = 2

prop_add      = x + f == fpx
prop_ assoc   = one * f + x * f == (one + x) * f
prop_powx     = x <^> (fieldModulus^degree - 1) == one
prop_powf     = f <^> (fieldModulus^degree - 1) == one
prop_div      = f / f == one
prop_divassoc = one / f + x / f == (one + x) / f

-- f*f*...*f == f^n
prop_powIterateFq a0 a1 n = n /= 0 ==>
  iterate (e*) e !! (n' - 1) == e <^> n'
  where
  e  = Fq2 (Fq a0) (Fq a1)
  n' = abs n

-- f * f^(n-1) == f^n
prop_powMultFq a0 a1 n = n /= 0 ==>
  e * (e <^> (n' - 1)) == e <^> n'
  where
  e  = Fq2 (Fq a0) (Fq a1)
  n' = abs n

return []
runTests = $quickCheckAll