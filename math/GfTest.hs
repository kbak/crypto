{-# LANGUAGE TemplateHaskell #-}
-- ^^^ for QuickCheck.quickCheckAll

module GfTest where

import Field
import Gf
import Test.QuickCheck

-- test modulus
tm = 137
tintGf n = intGf n tm

prop_mul = tintGf 2 * tintGf 2 == tintGf 4
prop_divAdd = tintGf 2 / tintGf 7 + tintGf 9 / tintGf 7 == tintGf 11 / tintGf 7
prop_mulAdd = tintGf 2 * tintGf 7 + tintGf 9 * tintGf 7 == tintGf 11 * tintGf 7
prop_pow = tintGf 9 <^> tm == tintGf 9
prop_inv = tintGf 9 / tintGf 9 == tintGf 1

prop_multiInvOne n = n > 0 ==> [inv n'] == multiInv [n']
    where
    n' = tintGf n

prop_multiInvMultiple ns = map inv ns' == multiInv ns'
    where
    ns' = map tintGf ns

return []
runTests = $quickCheckAll