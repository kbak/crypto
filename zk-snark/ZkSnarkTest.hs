{-# LANGUAGE TemplateHaskell #-}
-- ^^^ for QuickCheck.quickCheckAll
module ZkSnarkTest where
-- based on https://www.rareskills.io/post/quadratic-arithmetic-program
import QAP
import Curve
import KZG
import ZkSnark
import QAPTest
import Test.QuickCheck

prop_proveVerify s = ZkSnark.verify (g2, pairing) proof
    where
    secret         = 2 + abs s
    -- public setup: points, QAP matrices, and z polynomial
    (ptsG1, ptsG2) = trustedSetup secret g1 g2
    r1cs           = codeToR1cs variables flatCode
    witness        = getWitness variables assignment flatCode
    proof          = ZkSnark.prove (curveOrder, ptsG1, ptsG2) r1cs witness

return []
runTests = $quickCheckAll
