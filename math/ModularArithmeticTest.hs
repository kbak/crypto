{-# LANGUAGE TemplateHaskell #-}
-- ^^^ for QuickCheck.quickCheckAll

module ModularArithmeticTest where

import ModularArithmetic
import Test.QuickCheck

prop_multiInvOne n = n > 0 ==> [inv n' m] == multiInv [n'] m
    where
    m  = 13
    n' = n `mod` m

prop_multiInvMultiple ns = map (`inv` m) ns' == multiInv ns' m
    where
    m   = 13
    ns' = map (\n -> abs n `mod` m) ns

primes = filter isPrime [2 .. ] :: [Integer]

isPrime n = go 2
    where
    go d
        | d * d > n      = True
        | n `rem` d == 0 = False
        | otherwise      = go (d + 1)

return []
runTests = $quickCheckAll