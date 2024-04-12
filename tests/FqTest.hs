{-# LANGUAGE TemplateHaskell #-}
-- ^^^ for QuickCheck.quickCheckAll

module FqTest where

import Field
import FqConfig
import Fq
import Test.QuickCheck

prop_mul = 2 * 2 == Fq 4
prop_divAdd = 2 / 7 + 9 / 7 == Fq 11 / 7
prop_mulAdd = 2 * 7 + 9 * 7 == Fq 11 * 7
prop_pow = 9 <^> fieldModulus == Fq 9
prop_inv = 9 / 9 == Fq 1

prop_multiInvOne n = n > 0 ==> [inv n'] == multiInv [n']
    where
    n' = Fq n

prop_multiInvMultiple ns = map inv ns' == multiInv ns'
    where
    ns' = map (Fq . abs) ns

return []
runTests = $quickCheckAll