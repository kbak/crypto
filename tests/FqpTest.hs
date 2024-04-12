{-# LANGUAGE TemplateHaskell #-}
-- ^^^ for QuickCheck.quickCheckAll

module FqpTest where

import FqConfig
import Field
import Polynomial
import Fq
import Fqp
import Test.QuickCheck

-- checks that `modFqp` behaves in the same way as `polydiv`
prop_modFqp :: [Integer] -> [Integer] -> Property
prop_modFqp ps ms = withMaxSuccess 500 $ not (isZero msc) ==>
  polyclean (modFqp p) == polyclean r
  where
  (Fqp ps' ms') = newFqp ps ms
  psc = coeffclean ps'
  msc = coeffclean ms'
  -- ensures that the leading coefficient is exactly 1 (assumed by `modFqp`)
  oneMs = if last msc /= 1 then msc ++ [1] else msc
  p = Fqp psc oneMs
  r = Fqp (snd (psc `polydiv` oneMs)) oneMs

-- the inverse of 1 should be 1
prop_invOne = inv p == p
  where
  m = [1, 0, 1]
  p = newFqp [1] m

-- generates test inputs depending on the modulus
genInputsFq modulus = (x, f, fpx, one)
  where
  degree = fromIntegral $ length modulus - 1
  x      = newFqp [1] modulus
  f      = newFqp [1 .. degree] modulus
  fpx    = newFqp (2 : [2 .. degree]) modulus
  one    = toOne x

-- tests basic properties of the extension field
test_basicFq (x, f, fpx, one) =
  [ x + f == fpx
  , one * f + x * f == (one + x) * f
  , x <^> (fieldModulus^degree - 1) == cOne
  , f <^> (fieldModulus^degree - 1) == cOne
  , f / f == cOne
  , one / f + x / f == (one + x) / f
  ]
  where
  degree = length (pmod one) - 1
  cOne   = polyclean one

-- f*f*...*f == f^n
test_powIterateFq f n = n /= 0 ==>
  iterate (f*) f !! (n' - 1) == f <^> n'
  where
  n' = abs n

-- f * f^(n-1) == f^n
test_powMultFq :: Fqp -> Integer -> Property
test_powMultFq f n = n /= 0 ==>
  f * (f <^> (n' - 1)) == f <^> n'
  where
  n' = abs n

getF (_, f, _, _) = f

runFqpTests = do
  -- tests for FQ2
  let fq2Inputs = genInputsFq fq2Modulus
  mapM_ quickCheck $ test_basicFq fq2Inputs
  let fFq2 = getF fq2Inputs
  quickCheck $ test_powIterateFq fFq2
  quickCheck $ test_powMultFq fFq2
  -- tests for FQ12
  let fq12Inputs = genInputsFq fq12Modulus
  mapM_ quickCheck $ test_basicFq fq12Inputs
  let fFq12 = getF fq12Inputs
  quickCheck $ test_powIterateFq fFq12
  quickCheck $ test_powMultFq fFq12
  return True

return []
runTests = $quickCheckAll