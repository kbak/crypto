{-# LANGUAGE TemplateHaskell #-} 
-- ^^^ for QuickCheck.quickCheckAll
module ModularPolynomialTest where

import Polynomial
import Gf
import ModularPolynomial
import Test.QuickCheck

-- test modulus
tm = 137
tintGf n = intGf n tm

p = zPoly $ map tintGf [1..4]

prop_lagrange ns = ns' == [polyeval lPoly (tintGf (fromIntegral n)) | n <- [1 .. length ns']]
    where
    ns' = map tintGf ns
    lPoly = lagrangeGeneric (tintGf 1) (map tintGf [1..]) ns'

return []
runTests = $quickCheckAll